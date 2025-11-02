
SELECT 
  "t0"."id" AS "res0", 
  "t0"."name" AS "res2", 
  "t3"."id" AS "res17", 
  "t3"."title" AS "res18", 
  CASE WHEN ("t3"."id") = ("t7"."res0") THEN "t7"."res1" ELSE 0 END AS "res49", 
  CASE WHEN ("t3"."id") = ("t8"."res0") THEN "t8"."res1" ELSE 0 END AS "res50" 
FROM 
  "users" AS "t0" 
  LEFT JOIN "universities" AS "t1" ON ("t1"."id") = ("t0"."university__id") CROSS 
  JOIN "enrollments" AS "t2" CROSS 
  JOIN "courses" AS "t3" CROSS 
  JOIN "universities" AS "t4" CROSS 
  JOIN "users" AS "t5" 
  LEFT JOIN "universities" AS "t6" ON ("t6"."id") = ("t5"."university__id") LEFT 
  JOIN (
    SELECT 
      "t4"."id" AS "res0", 
      COUNT(*) AS "res1" 
    FROM 
      "lesson_completions" AS "t0" CROSS 
      JOIN "users" AS "t1" 
      LEFT JOIN "universities" AS "t2" ON ("t2"."id") = ("t1"."university__id") CROSS 
      JOIN "lessons" AS "t3" CROSS 
      JOIN "courses" AS "t4" CROSS 
      JOIN "universities" AS "t5" CROSS 
      JOIN "users" AS "t6" 
      LEFT JOIN "universities" AS "t7" ON ("t7"."id") = ("t6"."university__id") 
    WHERE 
      (
        (
          (
            (
              (
                ("t1"."id") = (6)
              ) 
              AND (
                ("t0"."user__id") = ("t1"."id")
              )
            ) 
            AND (
              ("t4"."university__id") = ("t5"."id")
            )
          ) 
          AND (
            ("t4"."instructor__id") = ("t6"."id")
          )
        ) 
        AND (
          ("t3"."chapter__course__id") = ("t4"."id")
        )
      ) 
      AND (
        ("t0"."lesson__id") = ("t3"."id")
      ) 
    GROUP BY 
      "t4"."id"
  ) AS "t7" ON "t7"."res0" = "t3"."id" LEFT 
  JOIN (
    SELECT 
      "t5"."id" AS "res0", 
      COUNT(*) AS "res1" 
    FROM 
      "submissions" AS "t0" CROSS 
      JOIN "users" AS "t1" 
      LEFT JOIN "universities" AS "t2" ON ("t2"."id") = ("t1"."university__id") CROSS 
      JOIN "exercises" AS "t3" CROSS 
      JOIN "lessons" AS "t4" CROSS 
      JOIN "courses" AS "t5" CROSS 
      JOIN "universities" AS "t6" CROSS 
      JOIN "users" AS "t7" 
      LEFT JOIN "universities" AS "t8" ON ("t8"."id") = ("t7"."university__id") 
    WHERE 
      (
        (
          (
            (
              (
                (
                  ("t0"."user__id") = ("t1"."id")
                ) 
                AND (
                  ("t5"."university__id") = ("t6"."id")
                )
              ) 
              AND (
                ("t5"."instructor__id") = ("t7"."id")
              )
            ) 
            AND (
              ("t4"."chapter__course__id") = ("t5"."id")
            )
          ) 
          AND (
            ("t3"."lesson__id") = ("t4"."id")
          )
        ) 
        AND (
          ("t0"."exercise__id") = ("t3"."id")
        )
      ) 
      AND (
        ("t1"."id") = (6)
      ) 
    GROUP BY 
      "t5"."id"
  ) AS "t8" ON "t8"."res0" = "t3"."id"
WHERE 
  (
    (
      (
        (
          ("t0"."id") = (6)
        ) 
        AND (
          ("t2"."user__id") = ("t0"."id")
        )
      ) 
      AND (
        ("t3"."university__id") = ("t4"."id")
      )
    ) 
    AND (
      ("t3"."instructor__id") = ("t5"."id")
    )
  ) 
  AND (
    ("t2"."course__id") = ("t3"."id")
  )

