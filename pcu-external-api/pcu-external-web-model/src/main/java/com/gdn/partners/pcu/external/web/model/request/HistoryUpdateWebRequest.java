package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class HistoryUpdateWebRequest {

  private String productSku;
  private String pickupPointCode;
  private String keyword;
  private Date startDate;
  private Date endDate;
  private String searchField;
  private boolean beforeOneMonths;
}
