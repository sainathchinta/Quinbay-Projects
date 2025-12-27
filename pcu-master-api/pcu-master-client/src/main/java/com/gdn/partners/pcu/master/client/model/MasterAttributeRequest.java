package com.gdn.partners.pcu.master.client.model;

import com.gdn.x.productcategorybase.dto.request.AttributeSortTypeRequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class MasterAttributeRequest extends AttributeRequest implements Serializable {

  private static final long serialVersionUID = -4672751429480292083L;

  private String nameEnglish;
  private String dsAttributeName;

  private byte[] descriptionEnglish;

  private AttributeSortTypeRequest sortType;

}
