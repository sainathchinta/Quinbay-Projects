package com.gda.mta.product.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class AutoApprovalProductHistoryDto {
  private List<ImageQcProcessingDto> imageQcProcessing;
  private SellerTypeDto sellerType;
  private CategoryPerformanceDto categoryPerformance;
  private String restrictedKeywords = "No";
}
