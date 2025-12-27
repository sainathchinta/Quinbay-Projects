package com.gdn.partners.pcu.external.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategorySuggestionWebResponse extends SimpleCategoryWebResponse {

  @Builder(builderMethodName = "categorySuggestionWebResponseBuilder")
  public CategorySuggestionWebResponse(
      String id, String categoryCode, String name, String nameEnglish, boolean display, boolean activated, boolean viewable,
      String parentCategoryId, long productCount, List<CategorySuggestionWebResponse> childCategories) {
    super(id, categoryCode, name, nameEnglish, display, activated, viewable, parentCategoryId);
    this.productCount = productCount;
    this.childCategories = childCategories;
  }

  private long productCount;
  private List<CategorySuggestionWebResponse> childCategories;
}
