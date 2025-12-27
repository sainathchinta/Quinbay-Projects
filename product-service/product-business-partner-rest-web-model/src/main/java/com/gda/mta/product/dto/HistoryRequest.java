package com.gda.mta.product.dto;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class HistoryRequest {
  private String productSku;
  private String searchField;
  private String keyword;
  private Date startDate;
  private Date endDate;
  private boolean beforeThreeMonths;
}
