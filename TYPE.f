      TYPE MATERIAL
        REAL WEIGHT
        INTEGER IPRICE
      END TYPE MATERIAL

      TYPE(MATERIAL) MAT

      MAT%WEIGHT = 10.
      MAT%IPRICE = 5
      WRITE(*,*) MAT%WEIGHT,MAT%IPRICE
      STOP
      END
