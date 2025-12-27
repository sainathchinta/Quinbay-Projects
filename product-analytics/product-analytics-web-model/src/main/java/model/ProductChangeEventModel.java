package model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductChangeEventModel extends GdnBaseDomainEventModel {
  private static final long serialVersionUID = -6484026229524605531L;
  private String productSku;
  private String productCode;
  private boolean markForDelete;
  private List<String> productChangeEventType = new ArrayList<>();
  private boolean archived;
  private boolean suspended;
  private boolean forceReview;
}