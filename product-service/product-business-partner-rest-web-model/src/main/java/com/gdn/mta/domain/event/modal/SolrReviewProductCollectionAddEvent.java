package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.solr.common.SolrInputDocument;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class SolrReviewProductCollectionAddEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -682209481652781599L;

  private List<SolrReviewProductCollectionAddEventFields> solrReviewProductCollectionAddEventFieldsList;
  private Map<String, Object> fieldsAndValues;
}
