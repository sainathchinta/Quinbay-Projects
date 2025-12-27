package com.gdn.partners.pcu.master.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CategoryReferenceWebResponse {
  private CategoryWebResponse masterCategoryResponse;
  private CategoryWebResponse salesCategoryResponse;
}
