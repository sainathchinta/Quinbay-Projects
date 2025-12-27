package com.gdn.partners.pcu.master.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class OscDetailsWebResponse {

  private String id;
  private String oscCode;
  private String oscShortText;
  private String oscLongText;
  private String storeId;
  private boolean activated;
  private List<MasterCategoryWebResponse> masterCategories;

}
