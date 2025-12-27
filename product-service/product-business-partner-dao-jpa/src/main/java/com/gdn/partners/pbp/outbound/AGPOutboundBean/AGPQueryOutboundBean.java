package com.gdn.partners.pbp.outbound.AGPOutboundBean;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.response.AgpResponseVo;
import com.gdn.partners.pbp.outbound.AGPQuery.AGPQueryFeign;
import com.gdn.partners.pbp.outbound.AGPQuery.AGPQueryOutbound;
import lombok.extern.slf4j.Slf4j;

import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.index.query.TermsQueryBuilder;
import org.elasticsearch.search.aggregations.AggregationBuilders;
import org.elasticsearch.search.aggregations.bucket.terms.TermsAggregationBuilder;
import org.elasticsearch.search.aggregations.metrics.ValueCountAggregationBuilder;
import org.elasticsearch.search.builder.SearchSourceBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.elasticsearch.index.query.QueryBuilders.termsQuery;

@Component
@Slf4j
public class AGPQueryOutboundBean implements AGPQueryOutbound {

  @Autowired
  private AGPQueryFeign agpQueryFeign;

  private final static String ORDER_ITEM_STATUS = "orderItemStatus";
  private final static String PRODUCT_SKU = "productGdnSku";
  private final static String VALUE = "value";
  private final static String COUNT = "count";
  private final static int START = 0;
  public static final List<String> ITEM_STATUS = Arrays.asList("X", "DF");


  @Override
  public Map<String, Boolean> findNumberOfOrderByProductSkuList(Set<String> productSkuList) {
    try {
      int size = Optional.ofNullable(productSkuList).orElse(new HashSet<>()).size();
      TermsQueryBuilder filterInactiveMatchQuery = QueryBuilders.termsQuery(ORDER_ITEM_STATUS, ITEM_STATUS);
      BoolQueryBuilder boolQuery =
          QueryBuilders.boolQuery().must(termsQuery(PRODUCT_SKU, productSkuList)).mustNot(filterInactiveMatchQuery);
      TermsAggregationBuilder avgAggregationBuilder = AggregationBuilders.terms(VALUE).field(PRODUCT_SKU);
      ValueCountAggregationBuilder valueCountAggregationBuilder = AggregationBuilders.count(COUNT).field(PRODUCT_SKU);
      String[] includeFields = new String[] {PRODUCT_SKU};
      String[] excludeFields = new String[] {};
      Map<String, Object> objectMap;
      objectMap = new ObjectMapper().readValue(
          SearchSourceBuilder.searchSource().from(START).size(size).query(boolQuery)
              .fetchSource(includeFields, excludeFields).aggregation(avgAggregationBuilder)
              .aggregation(valueCountAggregationBuilder).toString(), new TypeReference<Map<String, Object>>() {
          });
      AgpResponseVo aggregateQueryNativeResponse = agpQueryFeign.findNumberOfOrderByProductSkuList(objectMap);
      Map<String, Boolean> productSkuEligibilityMap = new HashMap<>();
      for (AgpResponseVo.Bucket bucket : aggregateQueryNativeResponse.getAggregations().getSkuTerms().getBuckets()) {
        productSkuEligibilityMap.put(bucket.getKey(), true);
      }
      return productSkuEligibilityMap;
    } catch (Exception ex) {
      log.error("#AGPServiceImpl findNumberOfOrder for product sku list : {}, error - ", productSkuList, ex);
      return new HashMap<>();
    }
  }
}
