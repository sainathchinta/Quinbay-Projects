package com.gdn.x.mta.distributiontask.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ProductAndReviewerDetailsDTO {

  private Product product;
  private ProductReviewer productReviewer;
}
