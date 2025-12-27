package com.gdn.x.productcategorybase.dto.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author BhagwatiMalav - created on 30/10/18
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class MasterAttributeRequest extends AttributeRequest{

  private static final long serialVersionUID = -4672751429480292083L;

  private String nameEnglish;
  private String dsAttributeName;

  private byte[] descriptionEnglish;

  private AttributeSortTypeRequest sortType;

}
