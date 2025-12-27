package com.gdn.partners.pcu.master.web.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
public class CategorySearchWebResponse {
  private String id;
  private String name;
  private String nameEnglish;
  private String categoryCode;
  private String parentCategoryId;
  private boolean activated;
  private boolean isLeafNode = false;
}
