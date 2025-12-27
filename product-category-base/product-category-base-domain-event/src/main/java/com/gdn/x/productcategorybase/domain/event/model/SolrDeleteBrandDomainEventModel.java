package com.gdn.x.productcategorybase.domain.event.model;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SolrDeleteBrandDomainEventModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 7536063852776695992L;
  private List<String> ids;
}
