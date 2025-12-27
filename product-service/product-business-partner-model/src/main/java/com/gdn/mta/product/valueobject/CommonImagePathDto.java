package com.gdn.mta.product.valueobject;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class CommonImagePathDto {
  private String locationPath;
  private boolean commonImage;
}
