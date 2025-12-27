package com.gdn.partners.pcu.master.web.model.response;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeDetailWebResponse extends AttributeMasterResponse {
  private String storeId;
  private Long version;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private boolean markForDelete;
  private List<AttributeValueWebResponse> attributeValues;
  private boolean screeningMandatory;
  private boolean variantCreatingUI;
}
