;;;
;;; scripts/inspect/hull.lisp
;;;

(tr-bind-current)

(lambda (isle x y)
  (let ((hp (room-hp isle x y)))
    (cond
      ((> hp 190)
       (case (weather)
         (weather-id-clear
          (dialog (tr "The hull gleams brightly in the cold air...")))

         (weather-id-sunshower
          (dialog (tr "The hull gleams brightly in the cold air. Light rain beads on the hull and rolls off...")))

         (weather-id-storm)

         (weather-id-rain
          (dialog (tr "The hull sheds sheets of water as rain falls on its gleaming surface...")))

         (weather-id-snow
          (dialog (tr "A fine dusting of frost coats the unblemished metal. The hull is cold and quiet...")))

         (weather-id-ash
          (dialog (tr "Radioactive particles bombard the hull, ash and dust settle in every crevice...")))

         (weather-id-night
          (dialog (tr "Moonlight catches in the hull's seams, the metal pale and ghostly in the dark.")))

         (weather-id-solar-storm
          (dialog (tr "Heat shimmers off the plates. The hull creaks and groans as it expands under the harsh light.")))))

      ((> hp 60)
       (dialog (tr "Scored and dented in places, it's seen its share of fights.")))

      (true
       (dialog (tr "The plating is buckled and scorched, fractured by many impacts..."))))))
