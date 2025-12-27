package com.gdn.partners.pcu.internal.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class MasterCategoryWebResponse {
  private String categoryCode;
  private String categoryName;
}
