package com.gdn.partners.pcu.master.web.model.request;

import java.util.Date;
import java.util.List;

import com.gdn.partners.pcu.master.web.model.AttributeValueUpdateWebModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Pradeep Reddy
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AttributeValuesUpdateWebRequest {

  private String sortType;
  private List<AttributeValueUpdateWebModel> attributeValues;
  private List<AttributeValueUpdateWebModel> addedAttributeValues;
  private List<AttributeValueUpdateWebModel> deletedAttributeValues;
  private String updatedBy;
  private Date updatedDate;
}
