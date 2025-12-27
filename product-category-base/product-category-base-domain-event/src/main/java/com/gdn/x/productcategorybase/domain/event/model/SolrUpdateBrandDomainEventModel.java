package com.gdn.x.productcategorybase.domain.event.model;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.x.productcategorybase.entity.solr.SolrUpdateBrandModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SolrUpdateBrandDomainEventModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 6127279109609273239L;
  private List<SolrUpdateBrandModel> updateBrandModels;

}
