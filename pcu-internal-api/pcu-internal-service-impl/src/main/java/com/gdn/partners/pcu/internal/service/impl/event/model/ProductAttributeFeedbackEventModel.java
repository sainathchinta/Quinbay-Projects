package com.gdn.partners.pcu.internal.service.impl.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import lombok.Builder;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

@EqualsAndHashCode
@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductAttributeFeedbackEventModel extends GdnBaseDomainEventModel
    implements Serializable {

  @Serial
  private static final long serialVersionUID = -5383104259015724292L;

  private String productCode;
  private String attributeName;
  private String attributeCode;
  private List<String> previousValue;
  private List<String> currentValue;
  private String username;
}