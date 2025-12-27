package com.gdn.x.productcategorybase.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UpdatedFieldsDTO {
  private boolean descriptionUpdated;
  private boolean categoryUpdated;
  private boolean productNameUpdated;
  private boolean brandUpdated;
}
