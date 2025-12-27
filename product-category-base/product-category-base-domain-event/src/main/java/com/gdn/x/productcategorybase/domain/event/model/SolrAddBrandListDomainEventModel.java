package com.gdn.x.productcategorybase.domain.event.model;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.x.productcategorybase.entity.solr.SolrBrandModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SolrAddBrandListDomainEventModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 2388846589016506171L;
  private List<SolrBrandModel> solrBrandModels;

}
