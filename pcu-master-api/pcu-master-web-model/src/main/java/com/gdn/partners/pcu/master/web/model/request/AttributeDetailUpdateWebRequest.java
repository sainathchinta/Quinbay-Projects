package com.gdn.partners.pcu.master.web.model.request;

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
public class AttributeDetailUpdateWebRequest {
  private String id;
  private String name;
  private String nameEnglish;
  private String attributeType;
  private byte[] description;
  private byte[] descriptionEnglish;
  private String example;
  private boolean searchAble;
  private boolean skuValue;
  private boolean isBasicView;
}
