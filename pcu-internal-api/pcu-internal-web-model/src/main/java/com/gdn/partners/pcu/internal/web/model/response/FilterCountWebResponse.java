package com.gdn.partners.pcu.internal.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class FilterCountWebResponse {

  private int yesterday;
  private int threeToFiveDays;
  private int twoDaysAgo;
  private int today;
  private int moreThanFiveDaysAgo;
  private int assigned;
  private int unassigned;
  private int revised;
  private int brandApproved;
  private int brandNotApproved;
  private boolean isSourceDb;
}
