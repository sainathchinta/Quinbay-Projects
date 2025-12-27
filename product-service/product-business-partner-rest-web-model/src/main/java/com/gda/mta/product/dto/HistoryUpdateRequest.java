package com.gda.mta.product.dto;

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
public class HistoryUpdateRequest {

   private String productSku;
   private String pickupPointCode;
   private String keyword;
   private Date startDate;
   private Date endDate;
   private String searchField;
   private boolean beforeOneMonths;
}
