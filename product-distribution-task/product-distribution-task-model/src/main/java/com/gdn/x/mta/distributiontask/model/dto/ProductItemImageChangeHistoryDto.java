package com.gdn.x.mta.distributiontask.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
public class ProductItemImageChangeHistoryDto {
  private Boolean originalImage;
  private String mainImage;
  private String images;
}
