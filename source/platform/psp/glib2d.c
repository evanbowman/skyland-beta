/*
 * gLib2D - A simple, fast, light-weight 2D graphics library.
 *
 * Copyright 2012 Clément Guérin <geecko.dev@free.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include "glib2d.h"

#include <pspkernel.h>
#include <pspdisplay.h>
#include <pspgu.h>
#include <vram.h>
#include <math.h>

#ifdef USE_PNG
#include <png.h>
#endif

#ifdef USE_JPEG
#include <jpeglib.h>
#endif

/* Defines */

#define DLIST_SIZE              (524288)
#define LINE_SIZE               (512)
#define PIXEL_SIZE              (4)
#define FRAMEBUFFER_SIZE        (LINE_SIZE*G2D_SCR_H*PIXEL_SIZE)
#define MALLOC_STEP             (128)
#define OBJ_COUNT               (MALLOC_STEP * 26)
#define TSTACK_MAX              (128)
#define SLICE_WIDTH             (64.f)
#define M_180_PI                (57.29578f)
#define M_PI_180                (0.017453292f)

#define DEFAULT_SIZE            (10)
#define DEFAULT_COORD_MODE      (G2D_UP_LEFT)
#define DEFAULT_X               (0.f)
#define DEFAULT_Y               (0.f)
#define DEFAULT_Z               (0.f)
#define DEFAULT_COLOR           (WHITE)
#define DEFAULT_ALPHA           (0xFF)

#define OBJ                     rctx.obj[rctx.n-1]
#define OBJ_I                   rctx.obj[i]
#define TRANSFORM               tstack[tstack_size-1]

/* Enumerations */

typedef enum
{
    RECTS, LINES, QUADS, POINTS
} Obj_Type;

/* Structures */

typedef struct
{
    float x, y, z;
    float rot, rot_sin, rot_cos;
    float scale_w, scale_h;
} Transform;

typedef struct
{
    float x, y, z;
    float rot_x, rot_y; // Rotation center
    float rot, rot_sin, rot_cos;
    int crop_x, crop_y;
    int crop_w, crop_h;
    float scale_w, scale_h;
    g2dColor color;
    g2dAlpha alpha;
} Object;

typedef struct
{
    Object *obj;
    Object cur_obj;
    unsigned int n;
    Obj_Type type;
    g2dTexture *tex;

    bool use_strip;
    bool use_z;
    bool use_vert_color;
    bool use_rot;
    bool use_tex_linear;
    bool use_tex_repeat;
    bool use_int;
    unsigned int color_count;
    g2dCoord_Mode coord_mode;
} RenderContext;

/* Local variables */

static int dlist[524288 / 4];

static RenderContext rctx;

static Transform tstack[TSTACK_MAX];
static unsigned int tstack_size;

static bool init = false;
static bool start = false;
static bool begin = false;
static bool zclear = true;
static bool scissor = false;

static float global_scale;

/* Global variables */

g2dTexture g2d_draw_buffer =
{
    512, 512,
    G2D_SCR_W, G2D_SCR_H,
    (float)G2D_SCR_W/G2D_SCR_H,
    false,
    (g2dColor*)FRAMEBUFFER_SIZE
};

g2dTexture g2d_disp_buffer =
{
    512, 512,
    G2D_SCR_W, G2D_SCR_H,
    (float)G2D_SCR_W/G2D_SCR_H,
    false,
    (g2dColor*)0
};

/* Internal functions */

void _g2dStart()
{
    if (!init)
        g2dInit();

    sceKernelDcacheWritebackRange(dlist, DLIST_SIZE);
    sceGuStart(GU_DIRECT, dlist);
    start = true;
}


void* _g2dSetVertex(void *vp, int i, float vx, float vy)
{
    // Vertex order: [texture uv] [color] [coord]
    short *vp_short;
    g2dColor *vp_color;
    float *vp_float;

    // Texture coordinates
    vp_short = (short*)vp;

    if (rctx.tex != NULL)
    {
        *(vp_short++) = OBJ_I.crop_x + vx * OBJ_I.crop_w;
        *(vp_short++) = OBJ_I.crop_y + vy * OBJ_I.crop_h;
    }

    // Color
    vp_color = (g2dColor*)vp_short;

    if (rctx.use_vert_color)
    {
        *(vp_color++) = OBJ_I.color;
    }

    // Coordinates
    vp_float = (float*)vp_color;

    vp_float[0] = OBJ_I.x;
    vp_float[1] = OBJ_I.y;

    if (rctx.type == RECTS)
    {
        vp_float[0] += vx * OBJ_I.scale_w;
        vp_float[1] += vy * OBJ_I.scale_h;

        if (rctx.use_rot) // Apply a rotation
        {
            float tx = vp_float[0] - OBJ_I.rot_x;
            float ty = vp_float[1] - OBJ_I.rot_y;

            vp_float[0] = OBJ_I.rot_x - OBJ_I.rot_sin*ty + OBJ_I.rot_cos*tx,
            vp_float[1] = OBJ_I.rot_y + OBJ_I.rot_cos*ty + OBJ_I.rot_sin*tx;
        }
    }

    if (rctx.use_int) // Pixel perfect
    {
        vp_float[0] = floorf(vp_float[0]);
        vp_float[1] = floorf(vp_float[1]);
    }
    vp_float[2] = OBJ_I.z;

    return (void*)(vp_float + 3);
}


#ifdef USE_VFPU
void vfpu_sincosf(float x, float *s, float *c)
{
    __asm__ volatile (
        "mtv    %2, s000\n"           // s000 = x
        "vcst.s s001, VFPU_2_PI\n"    // s001 = 2/pi
        "vmul.s s000, s000, s001\n"   // s000 = s000*s001
        "vrot.p c010, s000, [s, c]\n" // s010 = sinf(s000), s011 = cosf(s000)
        "mfv    %0, s010\n"           // *s = s010
        "mfv    %1, S011\n"           // *c = s011
        : "=r"(*s), "=r"(*c) : "r"(x)
    );
}
#endif

/* Main functions */

void g2dInit()
{
    if (init)
        return;

    // Display list allocation
    // dlist = malloc(DLIST_SIZE);
    /* if (dlist == NULL) { */
    /*     while (true) ; */
    /* } */

    // Setup GU
    sceGuInit();
    sceGuStart(GU_DIRECT, dlist);

    sceGuDrawBuffer(GU_PSM_8888, g2d_draw_buffer.data, LINE_SIZE);
    sceGuDispBuffer(G2D_SCR_W, G2D_SCR_H, g2d_disp_buffer.data, LINE_SIZE);
    sceGuDepthBuffer((void*)(FRAMEBUFFER_SIZE*2), LINE_SIZE);
    sceGuOffset(2048-G2D_SCR_W/2, 2048-G2D_SCR_H/2);
    sceGuViewport(2048, 2048, G2D_SCR_W, G2D_SCR_H);

    g2d_draw_buffer.data = vabsptr(g2d_draw_buffer.data);
    g2d_disp_buffer.data = vabsptr(g2d_disp_buffer.data);

    sceGuDepthRange(65535, 0);
    sceGuClearDepth(65535);
    sceGuAlphaFunc(GU_GREATER, 0, 255);
    sceGuDepthFunc(GU_LEQUAL);
    sceGuBlendFunc(GU_ADD, GU_SRC_ALPHA, GU_ONE_MINUS_SRC_ALPHA, 0, 0);
    sceGuTexFunc(GU_TFX_MODULATE, GU_TCC_RGBA);
    sceGuTexFilter(GU_LINEAR, GU_LINEAR);
    sceGuShadeModel(GU_SMOOTH);

    sceGuDisable(GU_CULL_FACE);
    sceGuDisable(GU_CLIP_PLANES);
    sceGuDisable(GU_DITHER);
    sceGuEnable(GU_ALPHA_TEST);
    sceGuEnable(GU_SCISSOR_TEST);
    sceGuEnable(GU_BLEND);

    g2dResetGlobalScale();
    g2dResetScissor();

    sceGuFinish();
    sceGuSync(0, 0);
    sceDisplayWaitVblankStart();
    sceGuDisplay(GU_TRUE);

    init = true;
}


void g2dTerm()
{
    if (!init)
        return;

    sceGuTerm();

    // free(dlist);

    init = false;
}

void g2dClear(g2dColor color)
{
    if (!start)
        _g2dStart();

    sceGuClearColor(color);
    sceGuClear(GU_COLOR_BUFFER_BIT |
               GU_FAST_CLEAR_BIT |
               (zclear ? GU_DEPTH_BUFFER_BIT : 0));

    zclear = false;
}


void g2dClearZ()
{
    if (!start)
        _g2dStart();

    sceGuClear(GU_DEPTH_BUFFER_BIT | GU_FAST_CLEAR_BIT);
    zclear = true;
}


static Object obj_buffer[OBJ_COUNT];


void _g2dBeginCommon(Obj_Type type, g2dTexture *tex)
{
    if (begin)
        return;

    if (!start)
        _g2dStart();

    // Reset render context
    rctx.obj = obj_buffer;
    rctx.n = 0;
    rctx.type = type;
    rctx.tex = tex;
    rctx.use_strip = false;
    rctx.use_z = false;
    rctx.use_vert_color = false;
    rctx.use_rot = false;
    rctx.use_tex_linear = true;
    rctx.use_tex_repeat = false;
    rctx.use_int = false;
    rctx.color_count = 0;
    rctx.coord_mode = DEFAULT_COORD_MODE;

    // Reset current object
    g2dReset();

    begin = true;
}


void g2dBeginRects(g2dTexture *tex)
{
    _g2dBeginCommon(RECTS, tex);
}


void g2dBeginLines(g2dLine_Mode mode)
{
    _g2dBeginCommon(LINES, NULL);

    rctx.use_strip = (mode & G2D_STRIP);
}


void g2dBeginQuads(g2dTexture *tex)
{
    _g2dBeginCommon(QUADS, tex);
}


void g2dBeginPoints()
{
    _g2dBeginCommon(POINTS, NULL);
}


void _g2dEndRects()
{
    // Define vertices properties
    int v_prim = (rctx.use_rot ? GU_TRIANGLES : GU_SPRITES);
    int v_obj_nbr = (rctx.use_rot ? 6 : 2);
    int v_nbr;
    int v_coord_size = 3;
    int v_tex_size = (rctx.tex != NULL ? 2 : 0);
    int v_color_size = (rctx.use_vert_color ? 1 : 0);
    int v_size = v_tex_size * sizeof(short) +
                 v_color_size * sizeof(g2dColor) +
                 v_coord_size * sizeof(float);
    int v_type = GU_VERTEX_32BITF | GU_TRANSFORM_2D;
    int i;

    if (rctx.tex != NULL)    v_type |= GU_TEXTURE_16BIT;
    if (rctx.use_vert_color) v_type |= GU_COLOR_8888;

    // Count how many vertices to allocate.
    if (rctx.tex == NULL || rctx.use_rot) // No slicing
    {
        v_nbr = v_obj_nbr * rctx.n;
    }
    else // Can use texture slicing for tremendous performance :)
    {
        v_nbr = 0;

        for (i=0; i<rctx.n; i++)
        {
            v_nbr += v_obj_nbr * ceilf(OBJ_I.crop_w/SLICE_WIDTH);
        }
    }

    // Allocate vertex list memory
    void *v = sceGuGetMemory(v_nbr * v_size);
    void *vi = v;

    // Build the vertex list
    for (i=0; i<rctx.n; i+=1)
    {
        if (rctx.use_rot) // Two triangles per object
        {
            vi = _g2dSetVertex(vi, i, 0.f, 0.f);
            vi = _g2dSetVertex(vi, i, 1.f, 0.f);
            vi = _g2dSetVertex(vi, i, 0.f, 1.f);
            vi = _g2dSetVertex(vi, i, 0.f, 1.f);
            vi = _g2dSetVertex(vi, i, 1.f, 0.f);
            vi = _g2dSetVertex(vi, i, 1.f, 1.f);
        }
        else if (rctx.tex == NULL) // One sprite per object
        {
            vi = _g2dSetVertex(vi, i, 0.f, 0.f);
            vi = _g2dSetVertex(vi, i, 1.f, 1.f);
        }
        else // Several sprites per object for a better texture cache use
        {
            float step = SLICE_WIDTH/OBJ_I.crop_w;
            float u;

            for (u=0.f; u<1.f; u+=step)
            {
                vi = _g2dSetVertex(vi, i, u, 0.f);
                vi = _g2dSetVertex(vi, i, (u+step>1.f ? 1.f : u+step), 1.f);
            }
        }
    }

    // Then put it in the display list.
    sceGuDrawArray(v_prim, v_type, v_nbr, NULL, v);
}


void _g2dEndLines()
{
    // Define vertices properties
    int v_prim = (rctx.use_strip ? GU_LINE_STRIP : GU_LINES);
    int v_obj_nbr = (rctx.use_strip ? 1 : 2);
    int v_nbr = v_obj_nbr * (rctx.use_strip ? rctx.n : rctx.n/2);
    int v_coord_size = 3;
    int v_color_size = (rctx.use_vert_color ? 1 : 0);
    int v_size = v_color_size * sizeof(g2dColor) +
                 v_coord_size * sizeof(float);
    int v_type = GU_VERTEX_32BITF | GU_TRANSFORM_2D;
    int i;

    if (rctx.use_vert_color) v_type |= GU_COLOR_8888;

    // Allocate vertex list memory
    void *v = sceGuGetMemory(v_nbr * v_size);
    void *vi = v;

    // Build the vertex list
    if (rctx.use_strip)
    {
        vi = _g2dSetVertex(vi, 0, 0.f, 0.f);

        for (i=1; i<rctx.n; i+=1)
        {
            vi = _g2dSetVertex(vi, i, 0.f, 0.f);
        }
    }
    else
    {
        for (i=0; i+1<rctx.n; i+=2)
        {
            vi = _g2dSetVertex(vi, i  , 0.f, 0.f);
            vi = _g2dSetVertex(vi, i+1, 0.f, 0.f);
        }
    }

    // Then put it in the display list.
    sceGuDrawArray(v_prim, v_type, v_nbr, NULL, v);
}


void _g2dEndQuads()
{
    // Define vertices properties
    int v_prim = GU_TRIANGLES;
    int v_obj_nbr = 6;
    int v_nbr = v_obj_nbr * (rctx.n / 4);
    int v_coord_size = 3;
    int v_tex_size = (rctx.tex != NULL ? 2 : 0);
    int v_color_size = (rctx.use_vert_color ? 1 : 0);
    int v_size = v_tex_size * sizeof(short) +
                 v_color_size * sizeof(g2dColor) +
                 v_coord_size * sizeof(float);
    int v_type = GU_VERTEX_32BITF | GU_TRANSFORM_2D;
    int i;

    if (rctx.tex != NULL)    v_type |= GU_TEXTURE_16BIT;
    if (rctx.use_vert_color) v_type |= GU_COLOR_8888;

    // Allocate vertex list memory
    void *v = sceGuGetMemory(v_nbr * v_size);
    void *vi = v;

    // Build the vertex list
    for (i=0; i+3<rctx.n; i+=4)
    {
        vi = _g2dSetVertex(vi, i  , 0.f, 0.f);
        vi = _g2dSetVertex(vi, i+1, 1.f, 0.f);
        vi = _g2dSetVertex(vi, i+3, 0.f, 1.f);
        vi = _g2dSetVertex(vi, i+3, 0.f, 1.f);
        vi = _g2dSetVertex(vi, i+1, 1.f, 0.f);
        vi = _g2dSetVertex(vi, i+2, 1.f, 1.f);
    }

    // Then put it in the display list.
    sceGuDrawArray(v_prim, v_type, v_nbr, NULL, v);
}


void _g2dEndPoints()
{
    // Define vertices properties
    int v_prim = GU_POINTS;
    int v_obj_nbr = 1;
    int v_nbr = v_obj_nbr * rctx.n;
    int v_coord_size = 3;
    int v_color_size = (rctx.use_vert_color ? 1 : 0);
    int v_size = v_color_size * sizeof(g2dColor) +
                 v_coord_size * sizeof(float);
    int v_type = GU_VERTEX_32BITF | GU_TRANSFORM_2D;
    int i;

    if (rctx.use_vert_color) v_type |= GU_COLOR_8888;

    // Allocate vertex list memory
    void *v = sceGuGetMemory(v_nbr * v_size);
    void *vi = v;

    // Build the vertex list
    for (i=0; i<rctx.n; i+=1)
    {
        vi = _g2dSetVertex(vi, i, 0.f, 0.f);
    }

    // Then put it in the display list.
    sceGuDrawArray(v_prim, v_type, v_nbr, NULL, v);
}


void g2dEnd()
{
    if (!begin || rctx.n == 0)
    {
        begin = false;
        return;
    }

    // Manage pspgu extensions

    if (rctx.use_z)
        sceGuEnable(GU_DEPTH_TEST);
    else
        sceGuDisable(GU_DEPTH_TEST);

    if (rctx.use_vert_color)
        sceGuColor(WHITE);
    else
        sceGuColor(rctx.cur_obj.color);

    if (rctx.tex == NULL)
        sceGuDisable(GU_TEXTURE_2D);
    else
    {
        sceGuEnable(GU_TEXTURE_2D);

        if (rctx.use_tex_linear) sceGuTexFilter(GU_LINEAR, GU_LINEAR);
        else                     sceGuTexFilter(GU_NEAREST, GU_NEAREST);

        if (rctx.use_tex_repeat) sceGuTexWrap(GU_REPEAT, GU_REPEAT);
        else                     sceGuTexWrap(GU_CLAMP, GU_CLAMP);

        // Load texture
        sceGuTexMode(GU_PSM_8888, 0, 0, rctx.tex->swizzled);
        sceGuTexImage(0, rctx.tex->tw, rctx.tex->th,
                      rctx.tex->tw, rctx.tex->data);
    }

    switch (rctx.type)
    {
        case RECTS:
            _g2dEndRects();
            break;

        case LINES:
            _g2dEndLines();
            break;

        case QUADS:
            _g2dEndQuads();
            break;

        case POINTS:
            _g2dEndPoints();
            break;
    }

    sceGuColor(WHITE);

    if (rctx.use_z)
        zclear = true;

    begin = false;
}


void g2dReset()
{
    g2dResetCoord();
    g2dResetScale();
    g2dResetColor();
    g2dResetAlpha();
    g2dResetRotation();
    g2dResetCrop();
    g2dResetTex();
}


void g2dFlip(g2dFlip_Mode mode)
{
    if (scissor)
        g2dResetScissor();

    sceGuFinish();
    sceGuSync(0, 0);

    if (mode & G2D_VSYNC)
        sceDisplayWaitVblankStart();

    g2d_disp_buffer.data = g2d_draw_buffer.data;
    g2d_draw_buffer.data = vabsptr(sceGuSwapBuffers());

    start = false;
}


void g2dAdd()
{
    if (!begin || rctx.cur_obj.scale_w == 0.f || rctx.cur_obj.scale_h == 0.f)
        return;


    if (rctx.n == OBJ_COUNT) {
        return;
    }

    rctx.n++;
    OBJ = rctx.cur_obj;

    // Coordinate mode stuff
    OBJ.rot_x = OBJ.x;
    OBJ.rot_y = OBJ.y;

    switch (rctx.coord_mode)
    {
        case G2D_UP_RIGHT:
            OBJ.x -= OBJ.scale_w;
            break;

        case G2D_DOWN_RIGHT:
            OBJ.x -= OBJ.scale_w;
            OBJ.y -= OBJ.scale_h;
            break;

        case G2D_DOWN_LEFT:
            OBJ.y -= OBJ.scale_h;
            break;

        case G2D_CENTER:
            OBJ.x -= OBJ.scale_w / 2.f;
            OBJ.y -= OBJ.scale_h / 2.f;
            break;

        case G2D_UP_LEFT:
        default:
            break;
    };

    // Alpha stuff
    OBJ.color = G2D_MODULATE(OBJ.color, 255, rctx.cur_obj.alpha);
}


void g2dPush()
{
    if (tstack_size >= TSTACK_MAX)
        return;

    tstack_size++;

    TRANSFORM.x = rctx.cur_obj.x;
    TRANSFORM.y = rctx.cur_obj.y;
    TRANSFORM.z = rctx.cur_obj.z;
    TRANSFORM.rot = rctx.cur_obj.rot;
    TRANSFORM.rot_sin = rctx.cur_obj.rot_sin;
    TRANSFORM.rot_cos = rctx.cur_obj.rot_cos;
    TRANSFORM.scale_w = rctx.cur_obj.scale_w;
    TRANSFORM.scale_h = rctx.cur_obj.scale_h;
}


void g2dPop()
{
    if (tstack_size <= 0)
        return;

    rctx.cur_obj.x = TRANSFORM.x;
    rctx.cur_obj.y = TRANSFORM.y;
    rctx.cur_obj.z = TRANSFORM.z;
    rctx.cur_obj.rot = TRANSFORM.rot;
    rctx.cur_obj.rot_sin = TRANSFORM.rot_sin;
    rctx.cur_obj.rot_cos = TRANSFORM.rot_cos;
    rctx.cur_obj.scale_w = TRANSFORM.scale_w;
    rctx.cur_obj.scale_h = TRANSFORM.scale_h;

    tstack_size--;

    if (rctx.cur_obj.rot != 0.f) rctx.use_rot = true;
    if (rctx.cur_obj.z != 0.f)   rctx.use_z = true;
}

/* Coord functions */

void g2dResetCoord()
{
    rctx.cur_obj.x = DEFAULT_X;
    rctx.cur_obj.y = DEFAULT_Y;
    rctx.cur_obj.z = DEFAULT_Z;
}


void g2dSetCoordMode(g2dCoord_Mode mode)
{
    if (mode > G2D_CENTER)
        return;

    rctx.coord_mode = mode;
}


void g2dGetCoordXYZ(float *x, float *y, float *z)
{
    if (x != NULL) *x = rctx.cur_obj.x;
    if (y != NULL) *y = rctx.cur_obj.y;
    if (z != NULL) *z = rctx.cur_obj.z;
}


void g2dSetCoordXY(float x, float y)
{
    rctx.cur_obj.x = x * global_scale;
    rctx.cur_obj.y = y * global_scale;
    rctx.cur_obj.z = 0.f;
}


void g2dSetCoordXYZ(float x, float y, float z)
{
    rctx.cur_obj.x = x * global_scale;
    rctx.cur_obj.y = y * global_scale;
    rctx.cur_obj.z = z * global_scale;

    if (z != 0.f)
        rctx.use_z = true;
}


void g2dSetCoordXYRelative(float x, float y)
{
    float inc_x = x;
    float inc_y = y;

    if (rctx.cur_obj.rot_cos != 1.f)
    {
        inc_x = -rctx.cur_obj.rot_sin*y + rctx.cur_obj.rot_cos*x;
        inc_y =  rctx.cur_obj.rot_cos*y + rctx.cur_obj.rot_sin*x;
    }

    rctx.cur_obj.x += inc_x * global_scale;
    rctx.cur_obj.y += inc_y * global_scale;
}


void g2dSetCoordXYZRelative(float x, float y, float z)
{
    g2dSetCoordXYRelative(x, y);

    rctx.cur_obj.z += z * global_scale;

    if (z != 0.f)
        rctx.use_z = true;
}


void g2dSetCoordInteger(bool use)
{
    rctx.use_int = use;
}

/* Scale functions */

void g2dResetGlobalScale()
{
    global_scale = 1.f;
}


void g2dResetScale()
{
    if (rctx.tex == NULL)
    {
        rctx.cur_obj.scale_w = DEFAULT_SIZE;
        rctx.cur_obj.scale_h = DEFAULT_SIZE;
    }
    else
    {
        rctx.cur_obj.scale_w = rctx.tex->w;
        rctx.cur_obj.scale_h = rctx.tex->h;
    }

    rctx.cur_obj.scale_w *= global_scale;
    rctx.cur_obj.scale_h *= global_scale;
}


void g2dGetGlobalScale(float *scale)
{
    if (scale != NULL)
        *scale = global_scale;
}


void g2dGetScaleWH(float *w, float *h)
{
    if (w != NULL) *w = rctx.cur_obj.scale_w;
    if (h != NULL) *h = rctx.cur_obj.scale_h;
}


void g2dSetGlobalScale(float scale)
{
    global_scale = scale;
}


void g2dSetScale(float w, float h)
{
    g2dResetScale();

    g2dSetScaleRelative(w, h);
}


void g2dSetScaleWH(float w, float h)
{
    rctx.cur_obj.scale_w = w * global_scale;
    rctx.cur_obj.scale_h = h * global_scale;

    // A trick to prevent an unexpected behavior when mirroring with GU_SPRITES.
    if (rctx.cur_obj.scale_w < 0 || rctx.cur_obj.scale_h < 0)
        rctx.use_rot = true;
}


void g2dSetScaleRelative(float w, float h)
{
    rctx.cur_obj.scale_w *= w;
    rctx.cur_obj.scale_h *= h;

    if (rctx.cur_obj.scale_w < 0 || rctx.cur_obj.scale_h < 0)
        rctx.use_rot = true;
}


void g2dSetScaleWHRelative(float w, float h)
{
    rctx.cur_obj.scale_w += w * global_scale;
    rctx.cur_obj.scale_h += h * global_scale;

    if (rctx.cur_obj.scale_w < 0 || rctx.cur_obj.scale_h < 0)
        rctx.use_rot = true;
}

/* Color functions */

void g2dResetColor()
{
    rctx.cur_obj.color = DEFAULT_COLOR;
}


void g2dResetAlpha()
{
    rctx.cur_obj.alpha = DEFAULT_ALPHA;
}


void g2dGetAlpha(g2dAlpha *alpha)
{
    if (alpha != NULL)
        *alpha = rctx.cur_obj.alpha;
}


void g2dSetColor(g2dColor color)
{
    rctx.cur_obj.color = color;

    if (++rctx.color_count > 1)
        rctx.use_vert_color = true;
}


void g2dSetAlpha(g2dAlpha alpha)
{
    if (alpha < 0)   alpha = 0;
    if (alpha > 255) alpha = 255;

    rctx.cur_obj.alpha = alpha;

    if (++rctx.color_count > 1)
        rctx.use_vert_color = true;
}


void g2dSetAlphaRelative(int alpha)
{
    g2dSetAlpha(rctx.cur_obj.alpha + alpha);
}

/* Rotation functions */

void g2dResetRotation()
{
    rctx.cur_obj.rot = 0.f;
    rctx.cur_obj.rot_sin = 0.f;
    rctx.cur_obj.rot_cos = 1.f;
}


void g2dGetRotationRad(float *radians)
{
    if (radians != NULL)
        *radians = rctx.cur_obj.rot;
}


void g2dGetRotation(float *degrees)
{
    if (degrees != NULL)
        *degrees = rctx.cur_obj.rot * M_180_PI;
}


void g2dSetRotationRad(float radians)
{
    if (radians == rctx.cur_obj.rot)
        return;

    rctx.cur_obj.rot = radians;

#ifdef USE_VFPU
    vfpu_sincosf(radians, &rctx.cur_obj.rot_sin, &rctx.cur_obj.rot_cos);
#else
    sincosf(radians, &rctx.cur_obj.rot_sin, &rctx.cur_obj.rot_cos);
#endif

    if (radians != 0.f)
        rctx.use_rot = true;
}


void g2dSetRotation(float degrees)
{
    g2dSetRotationRad(degrees * M_PI_180);
}


void g2dSetRotationRadRelative(float radians)
{
    g2dSetRotationRad(rctx.cur_obj.rot + radians);
}


void g2dSetRotationRelative(float degrees)
{
    g2dSetRotationRadRelative(degrees * M_PI_180);
}

/* Crop functions */

void g2dResetCrop()
{
    if (rctx.tex == NULL)
        return;

    rctx.cur_obj.crop_x = 0;
    rctx.cur_obj.crop_y = 0;
    rctx.cur_obj.crop_w = rctx.tex->w;
    rctx.cur_obj.crop_h = rctx.tex->h;
}


void g2dGetCropXY(int *x, int *y)
{
    if (rctx.tex == NULL)
        return;

    if (x != NULL) *x = rctx.cur_obj.crop_x;
    if (y != NULL) *y = rctx.cur_obj.crop_y;
}


void g2dGetCropWH(int *w, int *h)
{
    if (rctx.tex == NULL)
        return;

    if (w != NULL) *w = rctx.cur_obj.crop_w;
    if (h != NULL) *h = rctx.cur_obj.crop_h;
}


void g2dSetCropXY(int x, int y)
{
    if (rctx.tex == NULL)
        return;

    rctx.cur_obj.crop_x = x;
    rctx.cur_obj.crop_y = y;
}


void g2dSetCropWH(int w, int h)
{
    if (rctx.tex == NULL)
        return;

    rctx.cur_obj.crop_w = w;
    rctx.cur_obj.crop_h = h;
}


void g2dSetCropXYRelative(int x, int y)
{
    if (rctx.tex == NULL)
        return;

    g2dSetCropXY(rctx.cur_obj.crop_x + x, rctx.cur_obj.crop_y + y);
}


void g2dSetCropWHRelative(int w, int h)
{
    if (rctx.tex == NULL)
        return;

    g2dSetCropWH(rctx.cur_obj.crop_w + w, rctx.cur_obj.crop_h + h);
}

/* Texture functions */

void g2dResetTex()
{
    if (rctx.tex == NULL)
        return;

    rctx.use_tex_repeat = false;
    rctx.use_tex_linear = true;
}


void g2dSetTexRepeat(bool use)
{
    if (rctx.tex == NULL)
        return;

    rctx.use_tex_repeat = use;
}


void g2dSetTexLinear(bool use)
{
    if (rctx.tex == NULL)
        return;

    rctx.use_tex_linear = use;
}

/* Texture management */

unsigned int _getNextPower2(unsigned int n)
{
    n--;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;

    return n+1;
}


void _swizzle(unsigned char *dest, unsigned char *source, int width, int height)
{
    int i, j;
    int rowblocks = (width / 16);
    int rowblocks_add = (rowblocks-1) * 128;
    unsigned int block_address = 0;
    unsigned int *img = (unsigned int*)source;

    for (j=0; j<height; j++)
    {
        unsigned int *block = (unsigned int*)(dest + block_address);

        for (i = 0; i < rowblocks; i++)
        {
            *block++ = *img++;
            *block++ = *img++;
            *block++ = *img++;
            *block++ = *img++;

            block += 28;
        }

        if ((j & 0x7) == 0x7)
            block_address += rowblocks_add;

        block_address += 16;
    }
}


/* g2dTexture*g2dTexCreate(int w, int h) */
/* { */
/*     g2dTexture *tex = malloc(sizeof(g2dTexture)); */
/*     if (tex == NULL) */
/*         return NULL; */

/*     tex->tw = _getNextPower2(w); */
/*     tex->th = _getNextPower2(h); */
/*     tex->w = w; */
/*     tex->h = h; */
/*     tex->ratio = (float)w / h; */
/*     tex->swizzled = false; */

/*     tex->data = malloc(tex->tw * tex->th * sizeof(g2dColor)); */
/*     if (tex->data == NULL) */
/*     { */
/*         free(tex); */
/*         return NULL; */
/*     } */

/*     memset(tex->data, 0, tex->tw * tex->th * sizeof(g2dColor)); */

/*     return tex; */
/* } */


/* void g2dTexFree(g2dTexture **tex) */
/* { */
/*     if (tex == NULL) */
/*         return; */
/*     if (*tex == NULL) */
/*         return; */

/*     free((*tex)->data); */
/*     free((*tex)); */

/*     *tex = NULL; */
/* } */


#ifdef USE_PNG
/* g2dTexture* _g2dTexLoadPNG(FILE *fp) */
/* { */
/*     png_structp png_ptr; */
/*     png_infop info_ptr; */
/*     unsigned int sig_read = 0; */
/*     png_uint_32 width, height; */
/*     int bit_depth, color_type, interlace_type; */
/*     u32 x, y; */
/*     g2dColor *line; */
/*     g2dTexture *tex; */

/*     png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL); */
/*     png_set_error_fn(png_ptr, NULL, NULL, NULL); */
/*     info_ptr = png_create_info_struct(png_ptr); */
/*     png_init_io(png_ptr, fp); */
/*     png_set_sig_bytes(png_ptr, sig_read); */
/*     png_read_info(png_ptr, info_ptr); */
/*     png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type, */
/*                  &interlace_type, NULL, NULL); */
/*     png_set_strip_16(png_ptr); */
/*     png_set_packing(png_ptr); */

/*     if (color_type == PNG_COLOR_TYPE_PALETTE) */
/*         png_set_palette_to_rgb(png_ptr); */
/*     if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS)) */
/*         png_set_tRNS_to_alpha(png_ptr); */

/*     png_set_filler(png_ptr, 0xff, PNG_FILLER_AFTER); */

/*     tex = g2dTexCreate(width, height); */
/*     line = malloc(width * 4); */

/*     for (y = 0; y < height; y++) */
/*     { */
/*         png_read_row(png_ptr, (u8*) line, NULL); */

/*         for (x = 0; x < width; x++) */
/*             tex->data[x + y*tex->tw] = line[x]; */
/*     } */

/*     free(line); */
/*     png_read_end(png_ptr, info_ptr); */
/*     png_destroy_read_struct(&png_ptr, &info_ptr, NULL); */

/*     return tex; */
/* } */
#endif


#ifdef USE_JPEG
/* g2dTexture* _g2dTexLoadJPEG(FILE *fp) */
/* { */
/*     struct jpeg_decompress_struct dinfo; */
/*     struct jpeg_error_mgr jerr; */
/*     int width, height; */
/*     g2dTexture *tex; */
/*     u8 *line; */

/*     dinfo.err = jpeg_std_error(&jerr); */
/*     jpeg_create_decompress(&dinfo); */
/*     jpeg_stdio_src(&dinfo, fp); */
/*     jpeg_read_header(&dinfo, TRUE); */

/*     width = dinfo.image_width; */
/*     height = dinfo.image_height; */
/*     tex = g2dTexCreate(width, height); */
/*     line = malloc(width * 3); */

/*     jpeg_start_decompress(&dinfo); */

/*     if (dinfo.jpeg_color_space == JCS_GRAYSCALE) */
/*     { */
/*         while (dinfo.output_scanline < dinfo.output_height) */
/*         { */
/*             int y = dinfo.output_scanline; */
/*             int x; */

/*             jpeg_read_scanlines(&dinfo, &line, 1); */

/*             for (x=0; x<width; x++) */
/*             { */
/*                 g2dColor gray = line[x]; */
/*                 g2dColor c = gray | (gray << 8) | (gray << 16) | 0xff000000; */

/*                 tex->data[x + tex->tw * y] = c; */
/*             } */
/*         } */
/*     } */
/*     else */
/*     { */
/*         while (dinfo.output_scanline < dinfo.output_height) */
/*         { */
/*             int y = dinfo.output_scanline; */
/*             int x; */
/*             u8 *pline; */

/*             jpeg_read_scanlines(&dinfo, &line, 1); */
/*             pline = line; */

/*             for (x=0; x<width; x++) */
/*             { */
/*                 g2dColor c; */

/*                 c  = (*(pline++)); */
/*                 c |= (*(pline++)) << 8; */
/*                 c |= (*(pline++)) << 16; */

/*                 tex->data[x + tex->tw * y] = c | 0xff000000; */
/*             } */
/*         } */
/*     } */

/*     jpeg_finish_decompress(&dinfo); */
/*     jpeg_destroy_decompress(&dinfo); */
/*     free(line); */

/*     return tex; */
/* } */
#endif


/* g2dTexture* g2dTexLoad(char path[], g2dTex_Mode mode) */
/* { */
/*     g2dTexture *tex = NULL; */
/*     FILE *fp = NULL; */

/*     if (path == NULL) */
/*         return NULL; */
/*     if ((fp = fopen(path, "rb")) == NULL) */
/*         return NULL; */

/* #ifdef USE_PNG */
/*     if (strstr(path, ".png")) */
/*     { */
/*         tex = _g2dTexLoadPNG(fp); */
/*     } */
/* #endif */

/* #ifdef USE_JPEG */
/*     if (strstr(path, ".jpg") || strstr(path, ".jpeg")) */
/*     { */
/*         tex = _g2dTexLoadJPEG(fp); */
/*     } */
/* #endif */

/*     if (tex == NULL) */
/*         goto error; */

/*     fclose(fp); */
/*     fp = NULL; */

/*     // The PSP can't draw 512*512+ textures. */
/*     if (tex->w > 512 || tex->h > 512) */
/*         goto error; */

/*     // Swizzling is useless with small textures. */
/*     if ((mode & G2D_SWIZZLE) && (tex->w >= 16 || tex->h >= 16)) */
/*     { */
/*         u8 *tmp = malloc(tex->tw*tex->th*PIXEL_SIZE); */
/*         _swizzle(tmp, (u8*)tex->data, tex->tw*PIXEL_SIZE, tex->th); */
/*         free(tex->data); */
/*         tex->data = (g2dColor*)tmp; */
/*         tex->swizzled = true; */
/*     } */
/*     else */
/*         tex->swizzled = false; */

/*     sceKernelDcacheWritebackRange(tex->data, tex->tw*tex->th*PIXEL_SIZE); */

/*     return tex; */

/*     // Load failure... abort */
/* error: */
/*     if (fp != NULL) */
/*         fclose(fp); */

/*     g2dTexFree(&tex); */

/*     return NULL; */
/* } */

/* Scissor functions */

void g2dResetScissor()
{
    g2dSetScissor(0, 0, G2D_SCR_W, G2D_SCR_H);

    scissor = false;
}


void g2dSetScissor(int x, int y, int w, int h)
{
    sceGuScissor(x, y, x+w, y+h);

    scissor = true;
}

// EOF
