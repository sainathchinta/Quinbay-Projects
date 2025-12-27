package com.gdn.partners.pcu.external.client.helper;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class Paging {
  private int page;
  private int size;
  private long totalElements;
}
