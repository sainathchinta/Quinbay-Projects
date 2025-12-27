package com.gdn.partners.pcu.master.model.request;

import java.util.Date;
import java.util.List;

import com.gdn.partners.pcu.master.model.attribute.AttributeValueUpdateModel;
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
public class AttributeValuesUpdateServiceRequest {

  private String sortType;
  private List<AttributeValueUpdateModel> attributeValues;
  private List<AttributeValueUpdateModel> addedAttributeValues;
  private List<AttributeValueUpdateModel> deletedAttributeValues;
  private String updatedBy;
  private Date updatedDate;
}
