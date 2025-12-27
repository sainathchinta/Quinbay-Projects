package com.gdn.partners.pcu.master.client.model;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DimensionFilterRequest {
  private String keyword;
  private String sortedBy;
  private String sortDirection;
}
