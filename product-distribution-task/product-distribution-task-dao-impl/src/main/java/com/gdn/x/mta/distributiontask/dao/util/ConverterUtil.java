package com.gdn.x.mta.distributiontask.dao.util;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.model.solr.IprProductSolrFieldNames;
import com.gdn.x.mta.distributiontask.model.solr.SolrConstants;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.IntervalFacet;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.springframework.beans.BeanUtils;

import com.gda.mta.product.dto.DeleteProductRequest;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;

import static com.gdn.x.mta.distributiontask.model.Constants.BULK_REJECTION;

public class ConverterUtil {
  private static final String COMMA = ", ";
  private static final String HYPHEN = "-";

  public static DeleteProductRequest toDeleteProductRequest(RejectProductDTO rejectProductDTO) {
    DeleteProductRequest deleteProductRequest = new DeleteProductRequest();
    BeanUtils.copyProperties(rejectProductDTO, deleteProductRequest);
    if(Objects.nonNull(rejectProductDTO.getRejectReasonDto())) {
      String combinedReasons = new String();
      if (Objects.nonNull(rejectProductDTO.getRejectReasonDto().getProduct())) {
        for (String reason : rejectProductDTO.getRejectReasonDto().getProduct()) {
          combinedReasons = combinedReasons + reason + COMMA;
        }
      }
      if (Objects.nonNull(rejectProductDTO.getRejectReasonDto().getContent())) {
        for (String reason : rejectProductDTO.getRejectReasonDto().getContent()) {
          combinedReasons = combinedReasons + reason + COMMA;
        }
      }
      if (Objects.nonNull(rejectProductDTO.getRejectReasonDto().getImage())) {
        for (String reason : rejectProductDTO.getRejectReasonDto().getImage()) {
          combinedReasons = combinedReasons + reason + COMMA;
        }
      }
      if (StringUtils.isNotEmpty(combinedReasons)) {
        StringBuilder combinedReasonsStringBuffer = new StringBuilder(combinedReasons);
        combinedReasonsStringBuffer.delete(combinedReasonsStringBuffer.length() - 2, combinedReasonsStringBuffer.length());
        combinedReasons = combinedReasonsStringBuffer.toString();
        if (StringUtils.isNotEmpty(combinedReasons)) {
          combinedReasons = combinedReasons + HYPHEN + rejectProductDTO.getNotes();
        } else {
          combinedReasons = rejectProductDTO.getNotes();
        }
        deleteProductRequest.setNotes(combinedReasons);
      } else {
        deleteProductRequest.setNotes(rejectProductDTO.getNotes());
      }
    }
    if (rejectProductDTO.isBulkAction()) {
      deleteProductRequest.setNotes(BULK_REJECTION.concat(deleteProductRequest.getNotes()));
    }
    return deleteProductRequest;
  }

  public static void processIntervalFacets(Map<String, Object> countResponse,
    QueryResponse response) {
    List<IntervalFacet> intervalFacets = response.getIntervalFacets();
    Map<String, Integer> facetCountMap = intervalFacets.get(0).getIntervals().stream()
      .collect(Collectors.toMap(IntervalFacet.Count::getKey, IntervalFacet.Count::getCount));
    countResponse.put(SolrConstants.TODAY, facetCountMap.get(SolrConstants.TODAY_FACET_INTERVAL));
    countResponse.put(SolrConstants.YESTERDAY,
      facetCountMap.get(SolrConstants.YESTERDAY_FACET_INTERVAL));
    countResponse.put(SolrConstants.TWO_DAYS_AGO,
      facetCountMap.get(SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL));
    countResponse.put(SolrConstants.THREE_UNTIL_FIVE_DAYS_AGO,
      facetCountMap.get(SolrConstants.THREE_TO_FIVE_DAYS_AGO_FACET_INTERVAL));
    countResponse.put(SolrConstants.MORE_THAN_5_DAYS,
      facetCountMap.get(SolrConstants.FIVE_DAYS_AGO_FACET_INTERVAL));
  }

  public static void processStateFacetFieldsForIpr(Map<String, Object> countResponse,
    QueryResponse response) {
    for (FacetField facetField : response.getFacetFields()) {
      if (facetField.getName().equalsIgnoreCase(IprProductSolrFieldNames.STATE)) {
        for (FacetField.Count count : facetField.getValues()) {
          if (count.getName().equals(String.valueOf(ProductStateIPR.IN_REVIEW.getValue()))) {
            countResponse.put(ProductStateIPR.IN_REVIEW.name(), (int) count.getCount());
          } else if (count.getName()
            .equals(String.valueOf(ProductStateIPR.EVIDENCE_SUBMITTED.getValue()))) {
            countResponse.put(ProductStateIPR.EVIDENCE_SUBMITTED.name(), (int) count.getCount());
          }
        }
      }
    }
  }

  public static void extractAssignedAndUnassignedCounts(Map<String, Object> countResponse,
    QueryResponse response) {
    Map<String, Integer> facetQueryCounts = response.getFacetQuery();
    int assignedCount = facetQueryCounts.getOrDefault(SolrConstants.ASSIGNED_COUNT_QUERY, 0);
    int unassignedCount = facetQueryCounts.getOrDefault(SolrConstants.UNASSINGED_COUNT_QUERY, 0);
    countResponse.put(SolrConstants.ASSIGNED, assignedCount);
    countResponse.put(SolrConstants.UNASSIGNED, unassignedCount);
  }
}
