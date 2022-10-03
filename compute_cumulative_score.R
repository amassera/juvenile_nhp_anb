compute_cumulative_score <- function(df) {
  score_df<-data.frame()
  # Iterate through each subject
  for (subject in unique(df$ID)) {
    # Iterate through each session per subject
    
    # Get data for this subject, for this session, from the original 
    # data frame - only include completed trial (i.e. those with a 
    # response for CorrectIncorrect)
    session_data<-df[df$ID==subject & !is.na(df$CorrectIncorrect),]
    
    
    
    # Sum the delays for all correct change trials
    delay_sum<-sum(session_data$Delay[session_data$CorrectIncorrect==1 & session_data$Change==1])
    
    # Number of completed trials
    session_completed<-nrow(session_data)
    # Calculate 'Score', i.e. the sum of the delays for correct change 
    # trials divided by the number of completed trials
    session_score<-delay_sum/session_completed
    
    # Add new values to lists ready for the new data frame
    sub_score_df<-data.frame(ID=as.character(subject), 
                             Group=as.character(unique(session_data$Group)), 
                             Score=session_score)
    score_df<-rbind(score_df, sub_score_df)
  }
  return(score_df)
}