package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthorisationWipActionEventModel extends GdnBaseDomainEventModel
    implements Serializable {
  @Serial
  private static final long serialVersionUID = 3729740335202273592L;
  private String notificationType;
  private String businessPartnerCode;
  private String status;
  private String id;
  private List<NearExpiryModelEvent> nearExpiryModelEvents;
}
