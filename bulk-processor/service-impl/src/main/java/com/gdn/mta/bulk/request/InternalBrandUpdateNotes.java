package com.gdn.mta.bulk.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class InternalBrandUpdateNotes {
  private String sourceBrandCode;
  private String sourceBrandName;
  private String destinationBrandCode;
  private String destinationBrandName;
  private String destinationAttributeId;
}
