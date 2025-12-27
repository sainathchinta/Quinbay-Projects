package model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;
import java.util.Set;

@EqualsAndHashCode(callSuper = true)
@JsonIgnoreProperties(ignoreUnknown = true)
@Data
public class AttributeUpdateEventModel extends GdnBaseDomainEventModel
    implements Serializable {

  private static final long serialVersionUID = -5509103135712479425L;
  private String id;
  private String name;
  private String attributeCode;
  private String attributeType;
  private String description;
  private boolean skuValue;
  private boolean newAttribute;
  private boolean valueUpdate;
  private boolean variantCreation;
  private boolean mustShowOnCustomerSide;
  private String dsAttributeName;
  private Set<String> updatedFields;
  private boolean dsExtraction;
}
