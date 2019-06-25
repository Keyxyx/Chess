Public Class Form1

    Dim chess_board_not_for_moving(7, 7) As String
    Dim last_piece_clicked As String
    Dim black_moved_last As Boolean
    Dim white_moved_last As Boolean





    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim imaginary_but As Button
        For x As Integer = 0 To 7
            For y As Integer = 0 To 7
                imaginary_but = Me.Controls("button" & x & y)
                imaginary_but.Text = ""

            Next
        Next
        'this code takes out the text on the butttons so that when the form loads the buttons appear blank and do not
        'have the name of the buttons As there text , please do Not remove it needs to stay on form load for this reason

    End Sub

    Private Sub Start_button_Click(sender As Object, e As EventArgs) Handles Start_button.Click


        Call washing_green_color()
        Call ReadCSVFileToArray()
        Call Populate_the_borad()

    End Sub

    Private Sub ReadCSVFileToArray()
        Dim piece_name As String()

        Using importnames As New Microsoft.VisualBasic.FileIO.TextFieldParser("..\importnames.txt")
            importnames.TextFieldType = FileIO.FieldType.Delimited
            importnames.SetDelimiters(",")

            Dim t As Integer
            Dim y As Integer
            Dim x As Integer
            t = 0
            y = 0
            x = 0
            Do Until y = 8
                x = 0
                t = 0
                piece_name = importnames.ReadFields()
                Do Until x = 8

                    chess_board_not_for_moving(x, y) = piece_name(t)
                    t = t + 1
                    x = x + 1
                Loop
                y = y + 1
            Loop



            'For y As Integer = 0 To 7

            'piece_name = importnames.ReadFields()
            'piece_name(t) = chess_board_not_for_moving(0, y)

            'For x As Integer = 0 To 7
            'If x < 8 Then
            'piece_name(t) = chess_board_not_for_moving(x, y)
            't = t + 1

            'Else x = 0
            'End If
            'Next
            'Next

        End Using

    End Sub

    Private Sub Populate_the_borad()


        Dim Imaginary_but As Button


        For y As Integer = 0 To 7
            For x As Integer = 0 To 7
                Imaginary_but = Me.Controls("button" & x & y)
                Imaginary_but.Text = chess_board_not_for_moving(x, y)

            Next
        Next


    End Sub

    Private Sub Check_rules(ByVal x_coord_string As String, ByVal y_coord_string As String)
        Dim piece_code As String
        Dim piece_name As String




        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

        If black_moved_last = True And Mid(piece_name, 1, 1) = "B" Then
            TextBox1.Text = "Its not your move"
            Exit Sub
        End If

        If white_moved_last = True And Mid(piece_name, 1, 1) = "W" Then
            TextBox1.Text = "Its not your move"
            Exit Sub
        End If



        piece_code = Mid(piece_name, 3, 1)

        Select Case piece_code


            Case = "R"
                Call moving_straight(x_coord_string, y_coord_string)

            Case = "B"
                Call moving_diagonally(x_coord_string, y_coord_string)



            Case = "K"
                Call king_movement(x_coord_string, y_coord_string)
            Case = "Q"



                Call moving_diagonally(x_coord_string, y_coord_string)
                Call moving_straight(x_coord_string, y_coord_string)



            Case = "N"

                Call kNight_Movement(x_coord_string, y_coord_string)



                '''''''''''''''''''''''''''''''''''''''''''

            Case = ""

                ''''''''''''''''''''''''''''''''''''''''''''

            Case = "P"

                Call Pawn_movement(x_coord_string, y_coord_string)


                'check pawn movement , cant move after been moved once

        End Select





    End Sub

    Private Sub King_movement(x_coord_string, y_coord_string)
        Dim imaginary_button_local As Button
        Dim x_coord_saved As Integer
        Dim y_coord_saved As Integer
        Dim king_string As String

        x_coord_saved = x_coord_string
        y_coord_saved = y_coord_string



        imaginary_button_local = Me.Controls("button" & x_coord_saved & y_coord_saved)

        king_string = imaginary_button_local.Text
        Try
            imaginary_button_local = Me.Controls("button" & x_coord_string + 1 & y_coord_string)
            If imaginary_button_local.Text = "" Then
                imaginary_button_local.BackColor = Color.LightGreen

            ElseIf Mid(imaginary_button_local.Text, 1, 1) = Mid(king_string, 1, 1) Then

            ElseIf Mid(imaginary_button_local.Text, 1, 1) <> Mid(king_string, 1, 1) Then
                imaginary_button_local.BackColor = Color.LightGreen

            End If
            x_coord_string = x_coord_saved
            y_coord_string = y_coord_saved
        Catch ex As Exception

        End Try

        Try
            imaginary_button_local = Me.Controls("button" & x_coord_string - 1 & y_coord_string)
            If imaginary_button_local.Text = "" Then
                imaginary_button_local.BackColor = Color.LightGreen

            ElseIf Mid(imaginary_button_local.Text, 1, 1) = Mid(king_string, 1, 1) Then

            ElseIf Mid(imaginary_button_local.Text, 1, 1) <> Mid(king_string, 1, 1) Then
                imaginary_button_local.BackColor = Color.LightGreen


            End If
            x_coord_string = x_coord_saved
            y_coord_string = y_coord_saved

        Catch ex As Exception

        End Try


        Try
            imaginary_button_local = Me.Controls("button" & x_coord_string & y_coord_string - 1)
            If imaginary_button_local.Text = "" Then
                imaginary_button_local.BackColor = Color.LightGreen

            ElseIf Mid(imaginary_button_local.Text, 1, 1) = Mid(king_string, 1, 1) Then

            ElseIf Mid(imaginary_button_local.Text, 1, 1) <> Mid(king_string, 1, 1) Then
                imaginary_button_local.BackColor = Color.LightGreen


            End If
            x_coord_string = x_coord_saved
            y_coord_string = y_coord_saved

        Catch ex As Exception

        End Try

        Try
            imaginary_button_local = Me.Controls("button" & x_coord_string & y_coord_string + 1)
            If imaginary_button_local.Text = "" Then
                imaginary_button_local.BackColor = Color.LightGreen


            ElseIf Mid(imaginary_button_local.Text, 1, 1) = Mid(king_string, 1, 1) Then

            ElseIf Mid(imaginary_button_local.Text, 1, 1) <> Mid(king_string, 1, 1) Then
                imaginary_button_local.BackColor = Color.LightGreen

            End If
            x_coord_string = x_coord_saved
            y_coord_string = y_coord_saved
        Catch ex As Exception

        End Try

        Try
            imaginary_button_local = Me.Controls("button" & x_coord_string - 1 & y_coord_string + 1)
            If imaginary_button_local.Text = "" Then
                imaginary_button_local.BackColor = Color.LightGreen


            ElseIf Mid(imaginary_button_local.Text, 1, 1) = Mid(king_string, 1, 1) Then

            ElseIf Mid(imaginary_button_local.Text, 1, 1) <> Mid(king_string, 1, 1) Then
                imaginary_button_local.BackColor = Color.LightGreen

            End If
            x_coord_string = x_coord_saved
            y_coord_string = y_coord_saved
        Catch ex As Exception

        End Try


        Try
            imaginary_button_local = Me.Controls("button" & x_coord_string + 1 & y_coord_string - 1)
            If imaginary_button_local.Text = "" Then
                imaginary_button_local.BackColor = Color.LightGreen


            ElseIf Mid(imaginary_button_local.Text, 1, 1) = Mid(king_string, 1, 1) Then

            ElseIf Mid(imaginary_button_local.Text, 1, 1) <> Mid(king_string, 1, 1) Then
                imaginary_button_local.BackColor = Color.LightGreen

            End If
            x_coord_string = x_coord_saved
            y_coord_string = y_coord_saved
        Catch ex As Exception

        End Try


        Try
            imaginary_button_local = Me.Controls("button" & x_coord_string - 1 & y_coord_string - 1)
            If imaginary_button_local.Text = "" Then
                imaginary_button_local.BackColor = Color.LightGreen


            ElseIf Mid(imaginary_button_local.Text, 1, 1) = Mid(king_string, 1, 1) Then

            ElseIf Mid(imaginary_button_local.Text, 1, 1) <> Mid(king_string, 1, 1) Then
                imaginary_button_local.BackColor = Color.LightGreen

            End If
            x_coord_string = x_coord_saved
            y_coord_string = y_coord_saved

        Catch ex As Exception

        End Try

        Try
            imaginary_button_local = Me.Controls("button " & x_coord_string + 1 & y_coord_string - 1)
            If imaginary_button_local.Text = "" Then
                imaginary_button_local.BackColor = Color.LightGreen

            ElseIf Mid(imaginary_button_local.Text, 1, 1) = Mid(king_string, 1, 1) Then

            ElseIf Mid(imaginary_button_local.Text, 1, 1) <> Mid(king_string, 1, 1) Then
                imaginary_button_local.BackColor = Color.LightGreen



            End If
            x_coord_string = x_coord_saved
            y_coord_string = y_coord_saved

        Catch ex As Exception

        End Try

        Try
            imaginary_button_local = Me.Controls("button" & x_coord_string + 1 & y_coord_string + 1)
            If imaginary_button_local.Text = "" Then
                imaginary_button_local.BackColor = Color.LightGreen

            ElseIf Mid(imaginary_button_local.Text, 1, 1) = Mid(king_string, 1, 1) Then

            ElseIf Mid(imaginary_button_local.Text, 1, 1) <> Mid(king_string, 1, 1) Then
                imaginary_button_local.BackColor = Color.LightGreen

            End If
            x_coord_string = x_coord_saved
            y_coord_string = y_coord_saved
        Catch ex As Exception

        End Try

        imaginary_button_local = Me.Controls("button" & x_coord_string & y_coord_string)
        If imaginary_button_local.BackColor = Color.LightGreen Then


            Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)
        End If

    End Sub

    Private Sub Looking_for_win_lose_condition()
        Dim X As Integer
        Dim Y As Integer
        Dim imaginary_button_local As Button
        Dim king_is_not_here As Integer

        While king_is_not_here < 63
            For Y = 0 To 7
            For X = 0 To 7
                imaginary_button_local = Me.Controls("button" & X & Y)
                    If imaginary_button_local.Text = "W1K" Then


                    ElseIf imaginary_button_local.Text <> "W1K" Then
                        king_is_not_here += 1

                    ElseIf imaginary_button_local.Text <> "B1K" Then
                        king_is_not_here += 1

                    ElseIf imaginary_button_local.Text = "W1K" Then


                    End If
            Next
        Next
        End While
    End Sub
    Private Sub Pawn_movement(x_coord_string, y_coord_string)

        Dim imaginary_button_local As Button



        If Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) = "W" Then    ' is this piece a white piece, chess_borad_not_for_moiving = WP1 , so it looks at the 1st character and checks if its a W
            '''''''''''''








            Try
                If Mid(chess_board_not_for_moving(x_coord_string - 1, y_coord_string + 1), 1, 1) = "B" Then
                    imaginary_button_local = Me.Controls("button" & x_coord_string - 1 & y_coord_string + 1)
                    imaginary_button_local.BackColor = Color.LightGreen

                End If


            Catch ex As Exception

            End Try


            'If Mid(chess_board_not_for_moving(x_coord_string - 1, y_coord_string + 1), 1, 1) = "B" Then
            'imaginary_button_local = Me.Controls("button" & x_coord_string - 1 & y_coord_string + 1)
            'imaginary_button_local.BackColor = Color.LightGreen

            '        End If




            Try

                If Mid(chess_board_not_for_moving(x_coord_string + 1, y_coord_string + 1), 1, 1) = "B" Then
                    imaginary_button_local = Me.Controls("button" & x_coord_string + 1 & y_coord_string + 1)
                    imaginary_button_local.BackColor = Color.LightGreen
                End If

            Catch EX As Exception
            End Try

            Try 'this is made into a try so that it wont crash out because it trys to make a square that dosnt exist green 


                If chess_board_not_for_moving(x_coord_string, y_coord_string + 1) = "" Then  ' is the piece below this empty of other pieces? 

                    imaginary_button_local = Me.Controls("button" & x_coord_string & y_coord_string + 1)  'refrences the piece below our pawn 
                    imaginary_button_local.BackColor = Color.LightGreen


                    If y_coord_string = 1 Then

                        If chess_board_not_for_moving(x_coord_string, y_coord_string + 2) = "" Then
                            imaginary_button_local = Me.Controls("button" & x_coord_string & y_coord_string + 2)
                            imaginary_button_local.BackColor = Color.LightGreen

                        End If

                    End If



                End If


            Catch
            End Try

        Else
            '''''''''
            'this chunk is for black pawns 



            Try

                If Mid(chess_board_not_for_moving(x_coord_string - 1, y_coord_string - 1), 1, 1) = "W" Then
                    imaginary_button_local = Me.Controls("button" & x_coord_string - 1 & y_coord_string - 1)
                    imaginary_button_local.BackColor = Color.LightGreen

                End If
            Catch ex As Exception

            End Try


            Try
                If Mid(chess_board_not_for_moving(x_coord_string + 1, y_coord_string - 1), 1, 1) = "W" Then
                    imaginary_button_local = Me.Controls("button" & x_coord_string + 1 & y_coord_string - 1)
                    imaginary_button_local.BackColor = Color.LightGreen
                End If
            Catch ex As Exception

            End Try








            Try

                If chess_board_not_for_moving(x_coord_string, y_coord_string - 1) = "" Then

                    imaginary_button_local = Me.Controls("button" & x_coord_string & y_coord_string - 1)
                    imaginary_button_local.BackColor = Color.LightGreen



                    If y_coord_string = 6 Then

                        If chess_board_not_for_moving(x_coord_string, y_coord_string - 2) = "" Then
                            imaginary_button_local = Me.Controls("button" & x_coord_string & y_coord_string - 2)
                            imaginary_button_local.BackColor = Color.LightGreen

                        End If

                    End If


                End If

            Catch ex As Exception

            End Try
        End If


    End Sub
    Private Sub Finding_what_pieces_are_checking_the_king(X_coord_string, y_coord_String)
        Dim imaginary_button_local As Button
        Dim Color_of_piece_that_has_checked_a_king As String
        Dim Color_Of_piece_of_king_been_checked As String
        imaginary_button_local = Me.Controls("button" & X_coord_string & y_coord_String)





        imaginary_button_local = Me.Controls("button" & X_coord_string & y_coord_String)
        Color_of_piece_that_has_checked_a_king = Mid(imaginary_button_local.Text, 1, 1)



        If Color_of_piece_that_has_checked_a_king = "W" Then           'The if statement just determines the color of the king in chweck and the piece checking the king 
            Color_Of_piece_of_king_been_checked = "B"                  '
            '
        ElseIf Color_of_piece_that_has_checked_a_king = "B" Then
            Color_Of_piece_of_king_been_checked = "W"
        End If








        Dim x_coord_location_of_king As Integer
        Dim y_coord_location_of_king As Integer
        Dim Break_out_of_second_loops_variable As Integer


        For y_coord_location_of_king = 0 To 7      'this nested for statement finds the location of either the white  king 
            If Break_out_of_second_loops_variable = 1 Then
                Exit For
            Else

                For x_coord_location_of_king = 0 To 7
                    imaginary_button_local = Me.Controls("button" & x_coord_location_of_king & y_coord_location_of_king)
                    If Mid(imaginary_button_local.Text, 2, 2) = "1K" Then
                        Break_out_of_second_loops_variable += 1
                        Exit For
                    End If
                Next
            End If
        Next



        Dim x_coord_loco_king_original As Integer
        Dim y_coord_loco_king_original As Integer
        x_coord_loco_king_original = x_coord_location_of_king
        y_coord_loco_king_original = y_coord_location_of_king

        imaginary_button_local = Me.Controls("Button" & X_coord_string & y_coord_String)
        imaginary_button_local.BackColor = Color.Yellow


        imaginary_button_local = Me.Controls("button " & x_coord_location_of_king & y_coord_location_of_king - 1)
        imaginary_button_local.BackColor = Color.Yellow



        'x_coord_string is the piece that is checking the king 
        ' y_coord_string is the piece that is checking the king 
    End Sub


    Private Sub moving_straight(x_coord_string, y_coord_string)
        Dim Imaginary_button_local As Button
        Dim n_used_to_move_along As Integer





        Try
            For n_used_to_move_along = 1 To 7

                If Mid(chess_board_not_for_moving(x_coord_string + n_used_to_move_along, y_coord_string), 1, 1) = "" Then
                    Imaginary_button_local = Me.Controls("button" & x_coord_string + n_used_to_move_along & y_coord_string)
                    Imaginary_button_local.BackColor = Color.LightGreen


                ElseIf Mid(chess_board_not_for_moving(x_coord_string + n_used_to_move_along, y_coord_string), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    Exit For

                ElseIf Mid(chess_board_not_for_moving(x_coord_string + n_used_to_move_along, y_coord_string), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    Imaginary_button_local = Me.Controls("button" & x_coord_string + n_used_to_move_along & y_coord_string)
                    Imaginary_button_local.BackColor = Color.LightGreen


                    Imaginary_button_local = Me.Controls("button " & x_coord_string & y_coord_string)

                    If Mid(Imaginary_button_local.Text, 1, 1) <> Mid(Imaginary_button_local.Text = Me.Controls("button" & x_coord_string + n_used_to_move_along & y_coord_string), 1, 1) Then


                    End If
                    If Mid(chess_board_not_for_moving(x_coord_string + n_used_to_move_along, y_coord_string), 2, 2) = "1K" Then
                        Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)

                    End If

                    Exit For
                End If

            Next
        Catch ex As Exception
        End Try

        Try

            For n_used_to_move_along = 1 To 7
                If Mid(chess_board_not_for_moving(x_coord_string - n_used_to_move_along, y_coord_string), 1, 1) = "" Then
                    Imaginary_button_local = Me.Controls("button" & x_coord_string - n_used_to_move_along & y_coord_string)
                    Imaginary_button_local.BackColor = Color.LightGreen

                ElseIf Mid(chess_board_not_for_moving(x_coord_string - n_used_to_move_along, y_coord_string), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    Exit For

                ElseIf Mid(chess_board_not_for_moving(x_coord_string - n_used_to_move_along, y_coord_string), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    Imaginary_button_local = Me.Controls("button" & x_coord_string - n_used_to_move_along & y_coord_string)
                    Imaginary_button_local.BackColor = Color.LightGreen

                    If Mid(chess_board_not_for_moving(x_coord_string - n_used_to_move_along, y_coord_string), 2, 2) = "1K" Then
                        Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)

                    End If

                    Exit For

                End If

            Next

        Catch ex As Exception

        End Try

        Try
            For n_used_to_move_along = 1 To 7
                If Mid(chess_board_not_for_moving(x_coord_string, y_coord_string + n_used_to_move_along), 1, 1) = "" Then
                    Imaginary_button_local = Me.Controls("button" & x_coord_string & y_coord_string + n_used_to_move_along)
                    Imaginary_button_local.BackColor = Color.LightGreen


                ElseIf Mid(chess_board_not_for_moving(x_coord_string, y_coord_string + n_used_to_move_along), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    Exit For

                ElseIf Mid(chess_board_not_for_moving(x_coord_string, y_coord_string + n_used_to_move_along), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    Imaginary_button_local = Me.Controls("button" & x_coord_string & y_coord_string + n_used_to_move_along)
                    Imaginary_button_local.BackColor = Color.LightGreen

                    If Mid(chess_board_not_for_moving(x_coord_string, n_used_to_move_along + y_coord_string), 2, 2) = "1K" Then
                        Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)

                    End If

                    Exit For
                End If
            Next
        Catch ex As Exception

        End Try

        Try
            For n_used_to_move_along = 1 To 7
                If Mid(chess_board_not_for_moving(x_coord_string, y_coord_string - n_used_to_move_along), 1, 1) = "" Then
                    Imaginary_button_local = Me.Controls("button" & x_coord_string & y_coord_string - n_used_to_move_along)
                    Imaginary_button_local.BackColor = Color.LightGreen


                ElseIf Mid(chess_board_not_for_moving(x_coord_string, y_coord_string - n_used_to_move_along), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    Exit For

                ElseIf Mid(chess_board_not_for_moving(x_coord_string, y_coord_string - n_used_to_move_along), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    Imaginary_button_local = Me.Controls("button" & x_coord_string & y_coord_string - n_used_to_move_along)
                    Imaginary_button_local.BackColor = Color.LightGreen


                    If Mid(chess_board_not_for_moving(x_coord_string, n_used_to_move_along - y_coord_string), 2, 2) = "1K" Then
                        Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)

                    End If
                    Exit For
                End If
            Next
        Catch ex As Exception

        End Try





        'last piece clicked is the last piece that painted the squares green so we know this is where we started from. 
        'last piece clicked will be buttonXY

        ' target location is the place we've just clicked. it is diffrent to last piece clicked. last piece clicked is where we started from and target location is where we just clicked. 
        'target location will be buttonXY






        'If Button00.BackColor = Color.LightGreen Then
        'target_location = Button00.Name
        'Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string)
        'Else
        'Call washing_green_color(x_coord_string, y_coord_string)
        'last_piece_clicked = this_button.Name
        'Call Check_rules(x_coord_string, y_coord_string)
        'End If
    End Sub
    Private Sub moving_diagonally(ByVal x_coord_string As String, ByVal y_coord_string As String)
        Dim imaginary_button_local As Button
        Dim n_used_to_move_along As Integer

        Try
            For n_used_to_move_along = 1 To 7        '''''''''' to go right and down 
                If Mid(chess_board_not_for_moving(x_coord_string + n_used_to_move_along, y_coord_string + n_used_to_move_along), 1, 1) = "" Then
                    imaginary_button_local = Me.Controls("button" & x_coord_string + n_used_to_move_along & y_coord_string + n_used_to_move_along)
                    imaginary_button_local.BackColor = Color.LightGreen

                ElseIf Mid(chess_board_not_for_moving(x_coord_string + n_used_to_move_along, y_coord_string + n_used_to_move_along), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    Exit For

                ElseIf Mid(chess_board_not_for_moving(x_coord_string + n_used_to_move_along, y_coord_string + n_used_to_move_along), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    imaginary_button_local = Me.Controls("button" & x_coord_string + n_used_to_move_along & y_coord_string + n_used_to_move_along)
                    imaginary_button_local.BackColor = Color.LightGreen

                    If Mid(chess_board_not_for_moving(x_coord_string + n_used_to_move_along, n_used_to_move_along + y_coord_string), 2, 2) = "1K" Then
                        Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)

                    End If

                    Exit For
                End If
            Next
        Catch ex As Exception

        End Try


        Try
            For n_used_to_move_along = 1 To 7   ''''''''''''''''' to go left and down  -x +y
                If Mid(chess_board_not_for_moving(x_coord_string - n_used_to_move_along, y_coord_string + n_used_to_move_along), 1, 1) = "" Then
                    imaginary_button_local = Me.Controls("button" & x_coord_string - n_used_to_move_along & y_coord_string + n_used_to_move_along)
                    imaginary_button_local.BackColor = Color.LightGreen

                ElseIf Mid(chess_board_not_for_moving(x_coord_string - n_used_to_move_along, y_coord_string + n_used_to_move_along), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    Exit For

                ElseIf Mid(chess_board_not_for_moving(x_coord_string - n_used_to_move_along, y_coord_string + n_used_to_move_along), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    imaginary_button_local = Me.Controls("button" & x_coord_string - n_used_to_move_along & y_coord_string + n_used_to_move_along)
                    imaginary_button_local.BackColor = Color.LightGreen

                    If Mid(chess_board_not_for_moving(x_coord_string - n_used_to_move_along, n_used_to_move_along + y_coord_string), 2, 2) = "1K" Then
                        Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)

                    End If


                    Exit For

                End If
            Next
        Catch ex As Exception

        End Try

        Try
            For n_used_to_move_along = 1 To 7 ' to go right and up  +x -y
                If Mid(chess_board_not_for_moving(x_coord_string + n_used_to_move_along, y_coord_string - n_used_to_move_along), 1, 1) = "" Then
                    imaginary_button_local = Me.Controls("button" & x_coord_string + n_used_to_move_along & y_coord_string - n_used_to_move_along)
                    imaginary_button_local.BackColor = Color.LightGreen

                ElseIf Mid(chess_board_not_for_moving(x_coord_string + n_used_to_move_along, y_coord_string - n_used_to_move_along), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    Exit For

                ElseIf Mid(chess_board_not_for_moving(x_coord_string + n_used_to_move_along, y_coord_string - n_used_to_move_along), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    imaginary_button_local = Me.Controls("button" & x_coord_string + n_used_to_move_along & y_coord_string - n_used_to_move_along)
                    imaginary_button_local.BackColor = Color.LightGreen


                    If Mid(chess_board_not_for_moving(x_coord_string + n_used_to_move_along, n_used_to_move_along - y_coord_string), 2, 2) = "1K" Then
                        Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)

                    End If
                    Exit For
                End If
            Next
        Catch ex As Exception

        End Try


        Try
            For n_used_to_move_along = 1 To 7 ' to go left and up -x - y 
                If Mid(chess_board_not_for_moving(x_coord_string - n_used_to_move_along, y_coord_string - n_used_to_move_along), 1, 1) = "" Then
                    imaginary_button_local = Me.Controls("button" & x_coord_string - n_used_to_move_along & y_coord_string - n_used_to_move_along)
                    imaginary_button_local.BackColor = Color.LightGreen

                ElseIf Mid(chess_board_not_for_moving(x_coord_string - n_used_to_move_along, y_coord_string - n_used_to_move_along), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    Exit For

                ElseIf Mid(chess_board_not_for_moving(x_coord_string - n_used_to_move_along, y_coord_string - n_used_to_move_along), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                    imaginary_button_local = Me.Controls("button" & x_coord_string - n_used_to_move_along & y_coord_string - n_used_to_move_along)
                    imaginary_button_local.BackColor = Color.LightGreen


                    If Mid(chess_board_not_for_moving(x_coord_string - n_used_to_move_along, n_used_to_move_along - y_coord_string), 2, 2) = "1K" Then
                        Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)

                    End If
                    Exit For
                End If

            Next
        Catch ex As Exception

        End Try
    End Sub


    Private Sub washing_green_color()
        Dim imaginary_but As Button



        For y As Integer = 0 To 7
            For x As Integer = 0 To 7

                If (x + y) Mod 2 = 0 Then
                    imaginary_but = Me.Controls("button" & x & y)
                    imaginary_but.BackColor = Color.White
                Else
                    imaginary_but = Me.Controls("button" & x & y)
                    imaginary_but.BackColor = Color.Black
                End If




            Next
        Next






    End Sub

    Private Sub kNight_Movement(x_coord_string, y_coord_string)
        Dim imaginary_but As Button

        Try
            If Mid(chess_board_not_for_moving(x_coord_string + 2, y_coord_string + 1), 1, 1) = "" Then
                imaginary_but = Me.Controls("button" & x_coord_string + 2 & y_coord_string + 1)
                imaginary_but.BackColor = Color.LightGreen

            ElseIf Mid(chess_board_not_for_moving(x_coord_string + 2, y_coord_string + 1), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then

            ElseIf Mid(chess_board_not_for_moving(x_coord_string + 2, y_coord_string + 1), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                imaginary_but = Me.Controls("button" & x_coord_string + 2 & y_coord_string + 1)
                imaginary_but.BackColor = Color.LightGreen

                If Mid(chess_board_not_for_moving(x_coord_string + 2, y_coord_string + 1), 1, 1) = "1K" Then

                    Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)
                End If



            End If
        Catch ex As Exception

        End Try


        Try
            If Mid(chess_board_not_for_moving(x_coord_string + 2, y_coord_string - 1), 1, 1) = "" Then
                imaginary_but = Me.Controls("button" & x_coord_string + 2 & y_coord_string - 1)
                imaginary_but.BackColor = Color.LightGreen

            ElseIf Mid(chess_board_not_for_moving(x_coord_string + 2, y_coord_string - 1), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then

            ElseIf Mid(chess_board_not_for_moving(x_coord_string + 2, y_coord_string - 1), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                imaginary_but = Me.Controls("button" & x_coord_string + 2 & y_coord_string - 1)
                imaginary_but.BackColor = Color.LightGreen

                If Mid(chess_board_not_for_moving(x_coord_string + 2, y_coord_string - 1), 1, 1) = "1K" Then

                    Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)
                End If

            End If
        Catch ex As Exception

        End Try


        Try
            If Mid(chess_board_not_for_moving(x_coord_string - 2, y_coord_string + 1), 1, 1) = "" Then
                imaginary_but = Me.Controls("button" & x_coord_string - 2 & y_coord_string + 1)
                imaginary_but.BackColor = Color.LightGreen

            ElseIf Mid(chess_board_not_for_moving(x_coord_string - 2, y_coord_string + 1), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then

            ElseIf Mid(chess_board_not_for_moving(x_coord_string - 2, y_coord_string + 1), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                imaginary_but = Me.Controls("button" & x_coord_string - 2 & y_coord_string + 1)
                imaginary_but.BackColor = Color.LightGreen


                If Mid(chess_board_not_for_moving(x_coord_string - 2, y_coord_string + 1), 1, 1) = "1K" Then

                    Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)
                End If

            End If
        Catch ex As Exception

        End Try

        Try
            If Mid(chess_board_not_for_moving(x_coord_string - 2, y_coord_string - 1), 1, 1) = "" Then
                imaginary_but = Me.Controls("button" & x_coord_string - 2 & y_coord_string - 1)
                imaginary_but.BackColor = Color.LightGreen

            ElseIf Mid(chess_board_not_for_moving(x_coord_string - 2, y_coord_string - 1), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then


            ElseIf Mid(chess_board_not_for_moving(x_coord_string - 2, y_coord_string - 1), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                imaginary_but = Me.Controls("button" & x_coord_string - 2 & y_coord_string - 1)
                imaginary_but.BackColor = Color.LightGreen

                If Mid(chess_board_not_for_moving(x_coord_string - 2, y_coord_string - 1), 1, 1) = "1K" Then

                    Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)
                End If

            End If
        Catch ex As Exception

        End Try

        Try
            If Mid(chess_board_not_for_moving(x_coord_string - 1, y_coord_string + 2), 1, 1) = "" Then
                imaginary_but = Me.Controls("button" & x_coord_string - 1 & y_coord_string + 2)
                imaginary_but.BackColor = Color.LightGreen

            ElseIf Mid(chess_board_not_for_moving(x_coord_string - 1, y_coord_string + 2), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then

            ElseIf Mid(chess_board_not_for_moving(x_coord_string - 1, y_coord_string + 2), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                imaginary_but = Me.Controls("button" & x_coord_string - 1 & y_coord_string + 2)
                imaginary_but.BackColor = Color.LightGreen

                If Mid(chess_board_not_for_moving(x_coord_string - 1, y_coord_string + 2), 1, 1) = "1K" Then

                    Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)
                End If

            End If

        Catch ex As Exception

        End Try

        Try
            If Mid(chess_board_not_for_moving(x_coord_string + 1, y_coord_string + 2), 1, 1) = "" Then
                imaginary_but = Me.Controls("Button" & x_coord_string + 1 & y_coord_string + 2)
                imaginary_but.BackColor = Color.LightGreen

            ElseIf Mid(chess_board_not_for_moving(x_coord_string + 1, y_coord_string + 2), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then

            ElseIf Mid(chess_board_not_for_moving(x_coord_string + 1, y_coord_string + 2), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                imaginary_but = Me.Controls("button" & x_coord_string + 1 & y_coord_string + 2)
                imaginary_but.BackColor = Color.LightGreen

                If Mid(chess_board_not_for_moving(x_coord_string + 1, y_coord_string + 2), 1, 1) = "1K" Then

                    Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)
                End If

            End If
        Catch ex As Exception

        End Try

        Try
            If Mid(chess_board_not_for_moving(x_coord_string + 1, y_coord_string - 2), 1, 1) = "" Then
                imaginary_but = Me.Controls("button" & x_coord_string + 1 & y_coord_string - 2)
                imaginary_but.BackColor = Color.LightGreen

            ElseIf Mid(chess_board_not_for_moving(x_coord_string + 1, y_coord_string - 2), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then

            ElseIf Mid(chess_board_not_for_moving(x_coord_string + 1, y_coord_string - 2), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                imaginary_but = Me.Controls("button" & x_coord_string + 1 & y_coord_string - 2)
                imaginary_but.BackColor = Color.LightGreen

                If Mid(chess_board_not_for_moving(x_coord_string + 1, y_coord_string - 2), 1, 1) = "1K" Then

                    Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)
                End If



            End If
        Catch ex As Exception

        End Try


        Try
            If Mid(chess_board_not_for_moving(x_coord_string - 1, y_coord_string - 2), 1, 1) = "" Then
                imaginary_but = Me.Controls("button" & x_coord_string - 1 & y_coord_string - 2)
                imaginary_but.BackColor = Color.LightGreen

            ElseIf Mid(chess_board_not_for_moving(x_coord_string - 1, y_coord_string - 2), 1, 1) = Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then

            ElseIf Mid(chess_board_not_for_moving(x_coord_string - 1, y_coord_string - 2), 1, 1) <> Mid(chess_board_not_for_moving(x_coord_string, y_coord_string), 1, 1) Then
                imaginary_but = Me.Controls("button" & x_coord_string - 1 & y_coord_string - 2)
                imaginary_but.BackColor = Color.LightGreen


                If Mid(chess_board_not_for_moving(x_coord_string - 1, y_coord_string - 2), 1, 1) = "1K" Then

                    Call Finding_what_pieces_are_checking_the_king(x_coord_string, y_coord_string)
                End If

            End If
        Catch ex As Exception

        End Try




        'While (x_coord_string + n_used_to_scan) < 8 And (y_coord_string + n_used_to_scan) >= 0 And (x_coord_string - n_used_to_scan) <= 7 And (y_coord_string - n_used_to_scan) <= 7



    End Sub

    Private Sub moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)
        'last piece clicked is the last piece that painted the squares green so we know this is where we started from. 
        'last piece clicked will be buttonXY

        ' target location is the place we've just clicked. it is diffrent to last piece clicked. last piece clicked is where we started from and target location is where we just clicked. 
        'target location will be buttonXY

        'x_coord_string will be the x coordinate of the target button where we are going 
        'same for y_coord_string 

        Dim imaginary_button_local_variable_starting_location As Button
        Dim imaginary_button_local_variable_target_location As Button
        Dim x_coord_string_starting As String
        Dim y_coord_string_starting As String
        Dim piece_name As String
        imaginary_button_local_variable_starting_location = Me.Controls(last_piece_clicked)

        piece_name = imaginary_button_local_variable_starting_location.Text
        If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
            white_moved_last = False

            Exit Sub

        End If

        If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
            black_moved_last = False


            Exit Sub
        End If

        If Mid(piece_name, 1, 1) = "B" Then
            white_moved_last = False
            black_moved_last = True
        End If

        If Mid(piece_name, 1, 1) = "W" Then
            black_moved_last = False
            white_moved_last = True
        End If






        imaginary_button_local_variable_starting_location = Me.Controls(last_piece_clicked)
        imaginary_button_local_variable_target_location = Me.Controls(target_location)
        imaginary_button_local_variable_target_location.text = imaginary_button_local_variable_starting_location.Text
        imaginary_button_local_variable_starting_location.Text = ""


        x_coord_string_starting = Mid(last_piece_clicked, 7, 1)
        y_coord_string_starting = Mid(last_piece_clicked, 8, 1)
        chess_board_not_for_moving(x_coord_string, y_coord_string) = chess_board_not_for_moving(x_coord_string_starting, y_coord_string_starting)
        chess_board_not_for_moving(x_coord_string_starting, y_coord_string_starting) = ""




        piece_name = chess_board_not_for_moving(starting_x, starting_y)


        Call washing_green_color()



    End Sub
    Private Sub Button00_Click(sender As Object, e As EventArgs) Handles Button00.Click


        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name 'Eg button56 , not to be confused with the name of chess pieces 
        Dim x_coord_string As String
        Dim y_coord_string As String

        Dim target_location As String
        Dim starting_x As String
        Dim starting_y As String
        '  Dim chess_piece_at_this_button As String
        '  chess_piece_at_this_button = this_button.Text




        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)



        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)

        If Button00.BackColor = Color.LightGreen Then
            target_location = Button00.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)
            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)



            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False

                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If
        Else
            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If


        Call Looking_for_win_lose_condition()

        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button10_Click(sender As Object, e As EventArgs) Handles Button10.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name

        Dim x_coord_string As String
        Dim y_coord_string As String
        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button10.BackColor = Color.LightGreen Then
            target_location = Button10.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)
            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)



            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If
        Else
            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If




        Call Looking_for_win_lose_condition()

        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)



    End Sub

    Private Sub Button20_Click(sender As Object, e As EventArgs) Handles Button20.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name

        Dim piece_name As String
        piece_name = this_button.Name

        Dim x_coord_string As String
        Dim y_coord_string As String
        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)



        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 7, 1)




        Dim target_location As String

        If Button20.BackColor = Color.LightGreen Then
            target_location = Button20.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If
        Else
            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()


        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button30_Click(sender As Object, e As EventArgs) Handles Button30.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 7, 1)

        If Mid(this_button.Text, 1, 1) = "B" Then
            this_button.ForeColor = Color.Green
        Else this_button.ForeColor = Color.Red

        End If


        Dim target_location As String

        If Button30.BackColor = Color.LightGreen Then
            target_location = Button30.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)
            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)



            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If
        Else
            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)



    End Sub

    Private Sub Button40_Click(sender As Object, e As EventArgs) Handles Button40.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 7, 1)

        If Mid(this_button.Text, 1, 1) = "B" Then
            this_button.ForeColor = Color.Green
        Else this_button.ForeColor = Color.Red

        End If




        Dim target_location As String

        If Button40.BackColor = Color.LightGreen Then
            target_location = Button40.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)
            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)



            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If
        Else
            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()

        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button50_Click(sender As Object, e As EventArgs) Handles Button50.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 7, 1)

        If Mid(this_button.Text, 1, 1) = "B" Then
            this_button.ForeColor = Color.Green
        Else this_button.ForeColor = Color.Red

        End If




        Dim target_location As String

        If Button50.BackColor = Color.LightGreen Then
            target_location = Button50.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)
            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)



            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If
        Else
            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()


        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)
    End Sub

    Private Sub Button60_Click(sender As Object, e As EventArgs) Handles Button60.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)



        Dim target_location As String

        If Button60.BackColor = Color.LightGreen Then
            target_location = Button60.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)
            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)



            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If
        Else
            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If

        Call Looking_for_win_lose_condition()


        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub button70_Click(sender As Object, e As EventArgs) Handles button70.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name

        Dim piece_name As String




        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)

        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If button70.BackColor = Color.LightGreen Then
            target_location = button70.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)



            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If





        Else
            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If


        Call Looking_for_win_lose_condition()

        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button01_Click(sender As Object, e As EventArgs) Handles Button01.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)

        If Button01.BackColor = Color.LightGreen Then

            Call moving_pieces_into_box("button01", last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)
            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

          


            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If



        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)

        End If
        Call Looking_for_win_lose_condition()



        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Making_white_pieces_have_red_font(x_coord_string, y_coord_string)
        Dim imaginary_button As Button
        imaginary_button = Me.Controls("Button" & x_coord_string & y_coord_string)

        If Mid(imaginary_button.Text, 1, 1) = "B" Then
            imaginary_button.ForeColor = Color.Red
        Else
            imaginary_button.ForeColor = Color.Green
        End If





    End Sub

    Private Sub Button11_Click(sender As Object, e As EventArgs) Handles Button11.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name


        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)

        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button11.BackColor = Color.LightGreen Then
            target_location = Button11.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If
        Else


            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If


        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)
    End Sub

    Private Sub Button21_Click(sender As Object, e As EventArgs) Handles Button21.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button21.BackColor = Color.LightGreen Then
            target_location = Button21.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If


        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button31_Click(sender As Object, e As EventArgs) Handles Button31.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String


        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button31.BackColor = Color.LightGreen Then
            target_location = Button31.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)




            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If








            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If
        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button41_Click(sender As Object, e As EventArgs) Handles Button41.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)

        Dim piece_name As String
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button41.BackColor = Color.LightGreen Then
            target_location = Button41.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)
            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button51_Click(sender As Object, e As EventArgs) Handles Button51.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)

        Dim piece_name As String
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button51.BackColor = Color.LightGreen Then
            target_location = Button51.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)
            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button61_Click(sender As Object, e As EventArgs) Handles Button61.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)

        Dim piece_name As String
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button61.BackColor = Color.LightGreen Then
            target_location = Button61.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)
            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If


            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button71_Click(sender As Object, e As EventArgs) Handles Button71.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim piece_name As String
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button71.BackColor = Color.LightGreen Then
            target_location = Button71.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)
            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else
            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button02_Click(sender As Object, e As EventArgs) Handles Button02.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button02.BackColor = Color.LightGreen Then
            target_location = Button02.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button12_Click(sender As Object, e As EventArgs) Handles Button12.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button12.BackColor = Color.LightGreen Then
            target_location = Button12.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button22_Click(sender As Object, e As EventArgs) Handles Button22.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button22.BackColor = Color.LightGreen Then
            target_location = Button22.Name



            Dim piece_name As String
            piece_name = last_piece_clicked

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If


            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button32_Click(sender As Object, e As EventArgs) Handles Button32.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button32.BackColor = Color.LightGreen Then
            target_location = Button32.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button42_Click(sender As Object, e As EventArgs) Handles Button42.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button42.BackColor = Color.LightGreen Then
            target_location = Button42.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button52_Click(sender As Object, e As EventArgs) Handles Button52.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button52.BackColor = Color.LightGreen Then
            target_location = Button52.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button62_Click(sender As Object, e As EventArgs) Handles Button62.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button62.BackColor = Color.LightGreen Then
            target_location = Button62.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button72_Click(sender As Object, e As EventArgs) Handles Button72.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button72.BackColor = Color.LightGreen Then
            target_location = Button72.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button03_Click(sender As Object, e As EventArgs) Handles Button03.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button03.BackColor = Color.LightGreen Then
            target_location = Button03.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button13_Click(sender As Object, e As EventArgs) Handles Button13.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button13.BackColor = Color.LightGreen Then
            target_location = Button13.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button23_Click(sender As Object, e As EventArgs) Handles Button23.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button23.BackColor = Color.LightGreen Then
            target_location = Button23.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False

                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button33_Click(sender As Object, e As EventArgs) Handles Button33.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button33.BackColor = Color.LightGreen Then
            target_location = Button33.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button43_Click(sender As Object, e As EventArgs) Handles Button43.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button43.BackColor = Color.LightGreen Then
            target_location = Button43.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button53_Click(sender As Object, e As EventArgs) Handles Button53.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button53.BackColor = Color.LightGreen Then
            target_location = Button53.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button63_Click(sender As Object, e As EventArgs) Handles Button63.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button63.BackColor = Color.LightGreen Then
            target_location = Button63.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button73_Click(sender As Object, e As EventArgs) Handles Button73.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button73.BackColor = Color.LightGreen Then
            target_location = Button73.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button04_Click(sender As Object, e As EventArgs) Handles Button04.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button04.BackColor = Color.LightGreen Then
            target_location = Button04.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button14_Click(sender As Object, e As EventArgs) Handles Button14.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button14.BackColor = Color.LightGreen Then
            target_location = Button14.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button24_Click(sender As Object, e As EventArgs) Handles Button24.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button24.BackColor = Color.LightGreen Then
            target_location = Button24.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button34_Click(sender As Object, e As EventArgs) Handles Button34.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        Dim imaginary_but As Button

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        imaginary_but = Me.Controls(last_piece_clicked)
        Dim target_location As String

        If Button34.BackColor = Color.LightGreen Then
            target_location = Button34.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(imaginary_but.Text, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(imaginary_but.Text, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button44_Click(sender As Object, e As EventArgs) Handles Button44.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button44.BackColor = Color.LightGreen Then
            target_location = Button44.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button54_Click(sender As Object, e As EventArgs) Handles Button54.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name 'Eg button56 , not to be confused with the name of chess pieces 
        Dim x_coord_string As String
        Dim y_coord_string As String

        Dim target_location As String
        Dim starting_x As String
        Dim starting_y As String
        '  Dim chess_piece_at_this_button As String
        '  chess_piece_at_this_button = this_button.Text




        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)



        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)

        If Button54.BackColor = Color.LightGreen Then
            target_location = Button54.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)
            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)



            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False

                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If
        Else
            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If


        Call Looking_for_win_lose_condition()

        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button64_Click(sender As Object, e As EventArgs) Handles Button64.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button64.BackColor = Color.LightGreen Then
            target_location = Button64.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button74_Click(sender As Object, e As EventArgs) Handles Button74.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button74.BackColor = Color.LightGreen Then
            target_location = Button74.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button05_Click(sender As Object, e As EventArgs) Handles Button05.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button05.BackColor = Color.LightGreen Then
            target_location = Button05.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button15_Click(sender As Object, e As EventArgs) Handles Button15.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button15.BackColor = Color.LightGreen Then
            target_location = Button15.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button25_Click(sender As Object, e As EventArgs) Handles Button25.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button25.BackColor = Color.LightGreen Then
            target_location = Button25.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button35_Click(sender As Object, e As EventArgs) Handles Button35.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button35.BackColor = Color.LightGreen Then
            target_location = Button35.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)



    End Sub

    Private Sub Button45_Click(sender As Object, e As EventArgs) Handles Button45.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button45.BackColor = Color.LightGreen Then
            target_location = Button45.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)



    End Sub

    Private Sub Button55_Click(sender As Object, e As EventArgs) Handles Button55.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button55.BackColor = Color.LightGreen Then
            target_location = Button55.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button65_Click(sender As Object, e As EventArgs) Handles Button65.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button65.BackColor = Color.LightGreen Then
            target_location = Button65.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button75_Click(sender As Object, e As EventArgs) Handles Button75.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button75.BackColor = Color.LightGreen Then
            target_location = Button75.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button06_Click(sender As Object, e As EventArgs) Handles Button06.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button06.BackColor = Color.LightGreen Then
            target_location = Button06.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button16_Click(sender As Object, e As EventArgs) Handles Button16.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button16.BackColor = Color.LightGreen Then
            target_location = Button16.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button26_Click(sender As Object, e As EventArgs) Handles Button26.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button26.BackColor = Color.LightGreen Then
            target_location = Button26.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button36_Click(sender As Object, e As EventArgs) Handles Button36.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button36.BackColor = Color.LightGreen Then
            target_location = Button36.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)



    End Sub

    Private Sub Button46_Click(sender As Object, e As EventArgs) Handles Button46.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button46.BackColor = Color.LightGreen Then
            target_location = Button46.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button56_Click(sender As Object, e As EventArgs) Handles Button56.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button56.BackColor = Color.LightGreen Then
            target_location = Button56.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button66_Click(sender As Object, e As EventArgs) Handles Button66.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If Button66.BackColor = Color.LightGreen Then
            target_location = Button66.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub button76_Click(sender As Object, e As EventArgs) Handles button76.Click
        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name



        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)

        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)


        Dim target_location As String

        If button76.BackColor = Color.LightGreen Then
            target_location = button76.Name

            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_x, starting_y)

            Dim piece_name As String
            piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                TextBox1.Text = "Its not your move"
                Exit Sub
            End If



            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If


        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button07_Click(sender As Object, e As EventArgs) Handles Button07.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button07.BackColor = Color.LightGreen Then
            target_location = Button07.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)



    End Sub

    Private Sub Button17_Click(sender As Object, e As EventArgs) Handles Button17.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button17.BackColor = Color.LightGreen Then
            target_location = Button17.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)



    End Sub

    Private Sub Button27_Click(sender As Object, e As EventArgs) Handles Button27.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button27.BackColor = Color.LightGreen Then
            target_location = Button27.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button37_Click(sender As Object, e As EventArgs) Handles Button37.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button37.BackColor = Color.LightGreen Then
            target_location = Button37.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()

        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Button47_Click(sender As Object, e As EventArgs) Handles Button47.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button47.BackColor = Color.LightGreen Then
            target_location = Button47.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()

        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button57_Click(sender As Object, e As EventArgs) Handles Button57.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button57.BackColor = Color.LightGreen Then
            target_location = Button57.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()

        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)

    End Sub

    Private Sub Button67_Click(sender As Object, e As EventArgs) Handles Button67.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button67.BackColor = Color.LightGreen Then
            target_location = Button67.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()

        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub
    Private Sub Button77_Click(sender As Object, e As EventArgs) Handles Button77.Click

        Dim this_button As Button = CType(sender, Button)
        Dim Name_of_button_ As String = this_button.Name
        Dim piece_name As String
        Dim x_coord_string As String
        Dim y_coord_string As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)


        Dim starting_x As String
        Dim starting_y As String
        x_coord_string = Mid(Name_of_button_, 7, 1)
        y_coord_string = Mid(Name_of_button_, 8, 1)
        starting_x = Mid(Name_of_button_, 7, 1)
        starting_y = Mid(Name_of_button_, 8, 1)
        piece_name = chess_board_not_for_moving(x_coord_string, y_coord_string)
        Dim target_location As String

        If Button77.BackColor = Color.LightGreen Then
            target_location = Button77.Name
            Call moving_pieces_into_box(target_location, last_piece_clicked, x_coord_string, y_coord_string, starting_y, starting_x)

            If Mid(piece_name, 1, 1) = "B" And black_moved_last = True Then
                white_moved_last = False
                Exit Sub

            End If

            If Mid(piece_name, 1, 1) = "W" And white_moved_last = True Then
                black_moved_last = False
                Exit Sub
            End If




            If Mid(piece_name, 1, 1) = "B" Then
                black_moved_last = True
                white_moved_last = False

            ElseIf Mid(piece_name, 1, 1) = "W" Then
                white_moved_last = True
                black_moved_last = False

            End If

        Else

            Call washing_green_color()
            last_piece_clicked = this_button.Name
            Call Check_rules(x_coord_string, y_coord_string)
        End If
        Call Looking_for_win_lose_condition()
        Call Making_white_pieces_have_red_font(x_coord_string, y_coord_string)


    End Sub

    Private Sub Label8_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub Label1_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged

    End Sub
End Class

'put 4 blank rows in using empty string , use a double comma to  make a empty string in ccsv 
'once got this working Then save location Of 64 boxes  To csv , Then refine To only pieces that are alive thus makrs For development 

'pg 108 Is the start of section 8.2  

'strip out the x y  locations , use only the piece names , the location will be handled In the program 

'have each piece name 4 characters ong add spaces as buffers if need be. 
'will read In a full line at a time Then match it To the array , make it equal To the array. 
'rogers has sent code To your college email that will help , look at the current data arrays For help 
'lines 10 to 45 

'once got this working Then save location Of 64 boxes  To csv , then refine to only pieces that are alive thus makrs for development 
'pg 108 Is the start of section 8.