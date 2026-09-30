*>*******************************************************
       *> InCollege-UserSearch.cob
       *> Exact full-name lookup for saved profiles belonging to
       *> users other than the currently logged-in user. After a
       *> profile is shown, the user can send that person a
       *> connection request (via FREQ-SEND-REQUEST).
       *>*******************************************************
        >>SOURCE FORMAT FREE
        IDENTIFICATION DIVISION.
        PROGRAM-ID. USER-SEARCH.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY "InCollege-Common.cpy".
        COPY "InCollege-ProfileTable.cpy".
        01  WS-SEARCH-FULL-NAME         PIC X(100).
        01  WS-FOUND-IDX                PIC 9(2) VALUE 0.
        01  WS-MESSAGE                  PIC X(100).
        01  WS-TEMP-NAME                PIC X(100).
        01  WS-IDX                      PIC 9(2).
        01  WS-EXP-IDX                  PIC 9.
        01  WS-EDU-IDX                  PIC 9.

        01  WS-TARGET-USERNAME          PIC X(20).
        01  WS-ACTION-CHOICE            PIC X(100).
        01  WS-ACTION-DONE              PIC X VALUE 'N'.
            88  ACTION-DONE                      VALUE 'Y'.

        PROCEDURE DIVISION.
        USER-SEARCH-START.
            MOVE "Enter the full name of the person you are looking for:"
                TO WS-MESSAGE
            CALL "WRITE-LINE" USING WS-MESSAGE
            CALL "READ-LINE" USING WS-SEARCH-FULL-NAME

            IF END-OF-INPUT
                GOBACK
            END-IF

            MOVE 0 TO WS-FOUND-IDX

            IF WS-PROFILE-COUNT > 0
                PERFORM VARYING WS-IDX FROM 1 BY 1
                        UNTIL WS-IDX > WS-PROFILE-COUNT
                    IF FUNCTION TRIM(WS-PROF-USERNAME(WS-IDX)) NOT =
                       FUNCTION TRIM(WS-CURRENT-USERNAME)
                        MOVE SPACES TO WS-TEMP-NAME
                        STRING FUNCTION TRIM(WS-PROF-FIRST-NAME(WS-IDX))
                               " "
                               FUNCTION TRIM(WS-PROF-LAST-NAME(WS-IDX))
                               DELIMITED BY SIZE
                               INTO WS-TEMP-NAME
                        END-STRING
                        IF FUNCTION TRIM(WS-TEMP-NAME) =
                           FUNCTION TRIM(WS-SEARCH-FULL-NAME)
                            MOVE WS-IDX TO WS-FOUND-IDX
                        END-IF
                    END-IF
                END-PERFORM
            END-IF

            IF WS-FOUND-IDX > 0
                MOVE "--------------------" TO WS-MESSAGE
                CALL "WRITE-LINE" USING WS-MESSAGE
                MOVE SPACES TO WS-MESSAGE
                STRING "Profile of "
                       FUNCTION TRIM(WS-PROF-FIRST-NAME(WS-FOUND-IDX))
                       " "
                       FUNCTION TRIM(WS-PROF-LAST-NAME(WS-FOUND-IDX))
                       DELIMITED BY SIZE
                       INTO WS-MESSAGE
                END-STRING
                CALL "WRITE-LINE" USING WS-MESSAGE

                MOVE SPACES TO WS-MESSAGE
                STRING "Graduation Year: "
                       WS-PROF-GRAD-YEAR(WS-FOUND-IDX)
                       DELIMITED BY SIZE
                       INTO WS-MESSAGE
                END-STRING
                CALL "WRITE-LINE" USING WS-MESSAGE

                MOVE SPACES TO WS-MESSAGE
                STRING "Major: "
                       FUNCTION TRIM(WS-PROF-MAJOR(WS-FOUND-IDX))
                       DELIMITED BY SIZE
                       INTO WS-MESSAGE
                END-STRING
                CALL "WRITE-LINE" USING WS-MESSAGE

                MOVE SPACES TO WS-MESSAGE
                STRING "University: "
                       FUNCTION TRIM(WS-PROF-UNIVERSITY(WS-FOUND-IDX))
                       DELIMITED BY SIZE
                       INTO WS-MESSAGE
                END-STRING
                CALL "WRITE-LINE" USING WS-MESSAGE

                MOVE SPACES TO WS-MESSAGE
                STRING "About: "
                       FUNCTION TRIM(WS-PROF-ABOUT-ME(WS-FOUND-IDX))
                       DELIMITED BY SIZE
                       INTO WS-MESSAGE
                END-STRING
                CALL "WRITE-LINE" USING WS-MESSAGE

                MOVE "Experience:" TO WS-MESSAGE
                CALL "WRITE-LINE" USING WS-MESSAGE
                IF WS-PROF-EXP-COUNT(WS-FOUND-IDX) > 0
                    PERFORM VARYING WS-EXP-IDX FROM 1 BY 1
                            UNTIL WS-EXP-IDX > WS-PROF-EXP-COUNT(WS-FOUND-IDX)
                        MOVE SPACES TO WS-MESSAGE
                        STRING "  - "
                               FUNCTION TRIM(
                                 WS-PROF-EXP-TITLE(WS-FOUND-IDX,
                                                   WS-EXP-IDX))
                               " at "
                               FUNCTION TRIM(
                                 WS-PROF-EXP-COMPANY(WS-FOUND-IDX,
                                                  WS-EXP-IDX))
                               " ("
                               FUNCTION TRIM(
                                 WS-PROF-EXP-DATES(WS-FOUND-IDX,
                                                   WS-EXP-IDX))
                               ")"
                               DELIMITED BY SIZE
                               INTO WS-MESSAGE
                        END-STRING
                        CALL "WRITE-LINE" USING WS-MESSAGE

                        MOVE SPACES TO WS-MESSAGE
                        STRING "    Description: "
                               FUNCTION TRIM(
                                 WS-PROF-EXP-DESC(WS-FOUND-IDX,
                                                  WS-EXP-IDX))
                               DELIMITED BY SIZE
                               INTO WS-MESSAGE
                        END-STRING
                        CALL "WRITE-LINE" USING WS-MESSAGE
                    END-PERFORM
                ELSE
                    MOVE "  (None listed)" TO WS-MESSAGE
                    CALL "WRITE-LINE" USING WS-MESSAGE
                END-IF

                MOVE "Education:" TO WS-MESSAGE
                CALL "WRITE-LINE" USING WS-MESSAGE
                IF WS-PROF-EDU-COUNT(WS-FOUND-IDX) > 0
                    PERFORM VARYING WS-EDU-IDX FROM 1 BY 1
                            UNTIL WS-EDU-IDX > WS-PROF-EDU-COUNT(WS-FOUND-IDX)
                        MOVE SPACES TO WS-MESSAGE
                        STRING "  - "
                               FUNCTION TRIM(
                                 WS-PROF-EDU-DEGREE(WS-FOUND-IDX,
                                                    WS-EDU-IDX))
                               ", "
                               FUNCTION TRIM(
                                 WS-PROF-EDU-UNIVERSITY(WS-FOUND-IDX,
                                                    WS-EDU-IDX))
                               " ("
                               FUNCTION TRIM(
                                 WS-PROF-EDU-YEARS(WS-FOUND-IDX,
                                                   WS-EDU-IDX))
                               ")"
                               DELIMITED BY SIZE
                               INTO WS-MESSAGE
                        END-STRING
                        CALL "WRITE-LINE" USING WS-MESSAGE
                    END-PERFORM
                ELSE
                    MOVE "  (None listed)" TO WS-MESSAGE
                    CALL "WRITE-LINE" USING WS-MESSAGE
                END-IF

                MOVE "--------------------" TO WS-MESSAGE
                CALL "WRITE-LINE" USING WS-MESSAGE
                MOVE WS-PROF-USERNAME(WS-FOUND-IDX)
                    TO WS-TARGET-USERNAME
                PERFORM SEND-REQUEST-MENU
            ELSE
                MOVE "No one by that name could be found."
                    TO WS-MESSAGE
                CALL "WRITE-LINE" USING WS-MESSAGE
            END-IF
            GOBACK.

       *> Shown right after another user's profile is displayed.
       *> Keeps asking until the user picks 1 or 2 (or input ends).
        SEND-REQUEST-MENU.
            MOVE 'N' TO WS-ACTION-DONE.
            PERFORM UNTIL ACTION-DONE OR END-OF-INPUT
                MOVE "1. Send Connection Request" TO WS-MESSAGE
                CALL "WRITE-LINE" USING WS-MESSAGE
                MOVE "2. Back to Main Menu" TO WS-MESSAGE
                CALL "WRITE-LINE" USING WS-MESSAGE
                MOVE "Enter your choice:" TO WS-MESSAGE
                CALL "WRITE-LINE" USING WS-MESSAGE
                CALL "READ-LINE" USING WS-ACTION-CHOICE
                IF NOT END-OF-INPUT
                    EVALUATE FUNCTION TRIM(WS-ACTION-CHOICE)
                        WHEN "1"
                            CALL "FREQ-SEND-REQUEST"
                                USING WS-TARGET-USERNAME
                            MOVE 'Y' TO WS-ACTION-DONE
                        WHEN "2"
                            MOVE 'Y' TO WS-ACTION-DONE
                        WHEN OTHER
                            MOVE "Invalid choice, please try again."
                                TO WS-MESSAGE
                            CALL "WRITE-LINE" USING WS-MESSAGE
                    END-EVALUATE
                END-IF
            END-PERFORM.
        END PROGRAM USER-SEARCH.
