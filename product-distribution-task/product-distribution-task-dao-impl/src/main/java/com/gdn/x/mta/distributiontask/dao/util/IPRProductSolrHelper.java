package com.gdn.x.mta.distributiontask.dao.util;

import com.gdn.x.mta.distributiontask.model.solr.IprProductSolrFieldNames;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import com.gdn.x.mta.distributiontask.model.solr.SolrConstants;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.CommonParams;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class IPRProductSolrHelper {
  public static SolrInputDocument toIPRrSolrInputDocument(IPRProductSolr iprProductSolr) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(IprProductSolrFieldNames.STORE_ID, iprProductSolr.getStoreId());
    solrInputDocument.setField(IprProductSolrFieldNames.MARK_FOR_DELETE,
        iprProductSolr.isMarkForDelete());
    solrInputDocument.setField(IprProductSolrFieldNames.CREATED_DATE,
        iprProductSolr.getCreatedDate());
    solrInputDocument.setField(IprProductSolrFieldNames.CREATED_BY, iprProductSolr.getCreatedBy());
    solrInputDocument.setField(IprProductSolrFieldNames.UPDATED_DATE,
        iprProductSolr.getUpdatedDate());
    solrInputDocument.setField(IprProductSolrFieldNames.PRODUCT_CODE,
        iprProductSolr.getProductCode());
    solrInputDocument.setField(IprProductSolrFieldNames.PRODUCT_SKU,
        iprProductSolr.getProductSku());
    solrInputDocument.setField(IprProductSolrFieldNames.PRODUCT_NAME,
        iprProductSolr.getProductName());
    solrInputDocument.setField(IprProductSolrFieldNames.CATEGORY_CODE,
        iprProductSolr.getCategoryCode());
    solrInputDocument.setField(IprProductSolrFieldNames.CATEGORY_NAME,
        iprProductSolr.getCategoryName());
    solrInputDocument.setField(IprProductSolrFieldNames.BRAND_CODE, iprProductSolr.getBrandCode());
    solrInputDocument.setField(IprProductSolrFieldNames.BRAND_NAME, iprProductSolr.getBrandName());
    solrInputDocument.setField(IprProductSolrFieldNames.BUSINESS_PARTNER_CODE,
        iprProductSolr.getBusinessPartnerCode());
    solrInputDocument.setField(IprProductSolrFieldNames.PRODUCT_ADDED_DATE,
        iprProductSolr.getProductAddedDate());
    solrInputDocument.setField(IprProductSolrFieldNames.STATE,
      ProductStateIPR.getValueFromName(iprProductSolr.getState()));
    solrInputDocument.setField(IprProductSolrFieldNames.ASSIGNED_TO,
      iprProductSolr.getAssignedTo());
    solrInputDocument.setField(IprProductSolrFieldNames.ASSIGNED_DATE,
        iprProductSolr.getAssignedDate());
    solrInputDocument.setField(IprProductSolrFieldNames.SOURCE,
        iprProductSolr.getSource());
    return solrInputDocument;
  }

  public static SolrQuery getSolrQueryForPrimaryFilterCounts(String storeId) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(
      IprProductSolrFieldNames.STORE_ID + SolrConstants.COLON + appendDoubleQuotes(storeId));
    solrQuery.addFilterQuery(getQueryForMarkForDelete());
    List<String> facetFields = new ArrayList<>();
    facetFields.add(IprProductSolrFieldNames.STATE);
    facetFields.add(IprProductSolrFieldNames.ASSIGNED_TO);
    // Set up facet queries to count unassigned products (null or empty assignedTo)
    solrQuery.addFacetQuery(SolrConstants.ASSIGNED_COUNT_QUERY);
    solrQuery.addFacetQuery(SolrConstants.UNASSINGED_COUNT_QUERY);
    solrQuery.setStart(SolrConstants.ROW_ZERO);
    solrQuery.setRows(SolrConstants.ONE_ROW);
    solrQuery.setFacet(Boolean.TRUE);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    solrQuery.addIntervalFacets(IprProductSolrFieldNames.PRODUCT_ADDED_DATE,
      new String[] {SolrConstants.TODAY_FACET_INTERVAL, SolrConstants.YESTERDAY_FACET_INTERVAL,
        SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL,
        SolrConstants.THREE_TO_FIVE_DAYS_AGO_FACET_INTERVAL,
        SolrConstants.FIVE_DAYS_AGO_FACET_INTERVAL});
    solrQuery.addFacetField(
      Arrays.copyOf(facetFields.toArray(), facetFields.toArray().length, String[].class));
    return solrQuery;
  }

  public static String appendDoubleQuotes(String string) {
    return new StringBuilder(SolrConstants.DOUBLE_QUOTE).append(string)
      .append(SolrConstants.DOUBLE_QUOTE).toString();
  }

  private static String getQueryForMarkForDelete() {
    return new StringBuilder().append(IprProductSolrFieldNames.MARK_FOR_DELETE)
      .append(SolrConstants.COLON).append(false).toString();
  }

}
