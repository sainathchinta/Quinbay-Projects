package com.gdn.partners.pcu.external.web.model.request;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class HistorySummaryWebRequest {
  private static final long serialVersionUID = -1646986046317591187L;

  private String productSku;
  private String searchField;
  private String keyword;
  private Date startDate;
  private Date endDate;
  private boolean beforeThreeMonths;
}
