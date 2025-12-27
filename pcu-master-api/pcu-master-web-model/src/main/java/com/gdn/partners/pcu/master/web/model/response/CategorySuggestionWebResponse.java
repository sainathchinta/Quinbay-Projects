package com.gdn.partners.pcu.master.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class CategorySuggestionWebResponse {
  private String categoryLevel;
  private String categoryId;
  private String categoryCode;
  private String categoryName;
  private String categoryNameEnglish;
}
