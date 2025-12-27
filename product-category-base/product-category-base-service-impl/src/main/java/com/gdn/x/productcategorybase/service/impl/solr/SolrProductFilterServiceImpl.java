package com.gdn.x.productcategorybase.service.impl.solr;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.entity.solr.SolrProductModel;
import com.gdn.x.productcategorybase.entity.solr.SolrProductResponse;
import com.gdn.x.productcategorybase.service.solr.SolrProductFilterService;

/**
 * Created by Kesha on 02/05/16.
 */
@Service
public class SolrProductFilterServiceImpl implements SolrProductFilterService {

  private static final String CATEGORY_ID_FIELD = "category_id:";
  private static final String UPC_CODE_FIELD = "upc_codes:";
  private static final String QUERY_WITH_ALL_PARAMS = "*:*";
  private static final String OR_CLAUSE = " OR ";
  private static final String OPENING_BRACKET = "(";
  private static final String CLOSING_BRACKET = ")";
  private static final String CARET_SIGN = "^";
  private static final String NAME_FIELD = "name:";
  private static final String FUZZY_SEARCH_OPERATOR = "~";
  private static final String GROUP_CLAUSE = "group";
  private static final String GROUP_CLAUSE_FIELD = "group.field";
  private static final String PRODUCT_CODE = "product_code";
  private static final String PRODUCT_NAME = "name";
  private static final String SOLR_DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
  private static final String UPDATED_DATE_FIELD = "updated_date:";
  private static final String GREATER_THAN_BRACKET_START = "{";
  private static final String GREATER_THAN_BRACKET_END = " TO *}";
  private static final Integer MAX_RESULT_COUNT = 400000;
  private static final String PARENT_CATEGORY_ID_FIELD = "parent_category_id:";
  private static final String SCORE_ITEM = "score";
  private static final String DOUBLE_QUOTES_FOR_SOLR_QUERY = "\"";
  private static final String REVIEW_PENDING = "review_pending";
  private static final String COLON = ":";
  private static final String DASH_DELIMITER = "-";

  @Autowired
  @Qualifier("pcbCollectionClient")
  private CloudSolrClient productSolrClient;

  private static final Logger LOGGER = LoggerFactory.getLogger(SolrProductFilterServiceImpl.class);

  @Override
  public Set<SolrProductResponse> filterDuplicateProducts(SolrProductModel model, int size) {
    Matcher matcher = Constants.PATTERN_FOR_SPECIAL_CHARACTERS.matcher(model.getName());
    model.setName(StringUtils.trimToEmpty(matcher.replaceAll(StringUtils.SPACE)));
    return new HashSet<>(executeQuery(getQuery(model, size)));
  }

  /**
   * to get relevant search on the basis of category Id, upc code and name
   *
   * @param model must not null
   * @return
   */
  private SolrQuery getQuery(SolrProductModel model, int size) {
    SolrQuery query = new SolrQuery();
    List<String> queries = new ArrayList<>();
    int boostFactor = 2;
    queries.add(
        OPENING_BRACKET + UPC_CODE_FIELD + model.getUpcCode() + CLOSING_BRACKET + CARET_SIGN
            + boostFactor);
    queries.add(
        OPENING_BRACKET + NAME_FIELD + model.getName() + FUZZY_SEARCH_OPERATOR + CLOSING_BRACKET
            + CARET_SIGN + --boostFactor);
    query.setQuery(String.join(OR_CLAUSE, queries));
    query.setFilterQueries(PARENT_CATEGORY_ID_FIELD + DOUBLE_QUOTES_FOR_SOLR_QUERY + model.getFinalCategoryId()
        + DOUBLE_QUOTES_FOR_SOLR_QUERY);
    query.setRows(size);
    query.setFields(PRODUCT_CODE, PRODUCT_NAME);
    query.setSort(SCORE_ITEM, SolrQuery.ORDER.desc);
    return query;
  }

  private List<SolrProductResponse> executeQuery(SolrQuery query) {
    List<SolrProductResponse> responseList = new ArrayList<>();
    try {
      QueryResponse response = productSolrClient.query(query);
      for (SolrDocument document : response.getResults()) {
        SolrProductResponse model = new SolrProductResponse();
        model.setProductCode(document.getFieldValue(PRODUCT_CODE).toString());
        if (document.getFieldValue(PRODUCT_NAME) != null) {
          model.setProductName(document.getFieldValue(PRODUCT_NAME).toString());
        }
        responseList.add(model);
      }
    } catch (Exception e) {

      LOGGER.error("Solr Query Failed: Query = {} ", query, e);

    }
    return responseList;
  }

  private Page<SolrProductResponse> executePageableQuery(SolrQuery solrQuery, int page, int size) {
    try {
      QueryResponse response = productSolrClient.query(solrQuery);

      List<SolrProductResponse> responseList = new ArrayList<>();
      for (SolrDocument document : response.getResults()) {
        SolrProductResponse model = SolrProductResponse.builder()
          .productCode(document.getFieldValue(PRODUCT_CODE).toString())
          .productName(Optional.ofNullable(document.getFieldValue(PRODUCT_NAME)).map(String::valueOf).orElse(null))
          .build();
        responseList.add(model);
      }
      return new PageImpl<>(responseList, PageRequest.of(page, size), response.getResults().getNumFound());
    } catch(Exception e) {
      LOGGER.error("Solr pageable Query Failed: Query = {} ", solrQuery, e);
    }
    return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
  }

  @Override
  public Page<SolrProductResponse> getActiveProductIds(SolrProductModel model, int page, int size) {
    SolrQuery query = new SolrQuery();

    query.setFields(PRODUCT_CODE);
    query.setQuery(QUERY_WITH_ALL_PARAMS);
    query.setFilterQueries(
        CATEGORY_ID_FIELD + DOUBLE_QUOTES_FOR_SOLR_QUERY + model.getProductCategoryId() + DOUBLE_QUOTES_FOR_SOLR_QUERY);
    query.setStart(page * size + 1);
    query.setRows(size);

    if (model.getUpdatedDate() != null) {
      String updatedDate = new SimpleDateFormat(SOLR_DATE_TIME_FORMAT).format(model.getUpdatedDate());
      String updatedDateClause = new StringBuilder(UPDATED_DATE_FIELD)
        .append(GREATER_THAN_BRACKET_START)
        .append(updatedDate)
        .append(GREATER_THAN_BRACKET_END)
        .toString();
      query.setQuery(updatedDateClause);
    }
    return executePageableQuery(query, page, size);
  }

}

