package com.gdn.x.productcategorybase.service.impl.solr;


import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.params.SolrParams;
import org.apache.solr.common.util.NamedList;
import org.apache.solr.common.util.SimpleOrderedMap;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.productcategorybase.entity.solr.AttributeModel;
import com.gdn.x.productcategorybase.entity.solr.SolrProductModel;

/**
 * Created by arie.prastowo on 9/6/2016.
 */
public class SolrProductFilterServiceImplTest {

  private final String NAME = "NAME+?";
  private final String NAME_CLEANED_FUZZY = "NAME~";
  private final String UPC_CODE = "UPC CODE";
  private final String FINAL_CATEGORY_ID = "FINAL_CATEGORY_ID";
  private static final String CATEGORY_ID_FIELD = "category_id:";
  private final int GROUP_MATCHES = 4;
  private static final String DOUBLE_QUOTES_FOR_SOLR_QUERY = "\"";

  @InjectMocks
  private SolrProductFilterServiceImpl solrProductFilterService;

  @Mock
  private CloudSolrClient solrClient;

  @Mock
  private QueryResponse queryResponse;

  private SolrProductModel solrProductModel;

  @Captor
  private ArgumentCaptor<SolrParams> solrParamsArgumentCaptor;

  @Captor
  private ArgumentCaptor<SolrQuery> solrQueryArgumentCaptor;

  @BeforeEach
  public void initTest() {
    MockitoAnnotations.initMocks(this);
  }

  private List<AttributeModel> generateAttributeModelList() {
    AttributeModel attributeModel1 = new AttributeModel("Model name 1", "Model value 1");
    AttributeModel attributeModel2 = new AttributeModel("Model name 2", "Model value 2");
    AttributeModel attributeModel3 = new AttributeModel("brand", "Model value 3");
    List<AttributeModel> attributeModelList = new ArrayList<AttributeModel>();
    attributeModelList.add(attributeModel1);
    attributeModelList.add(attributeModel2);
    attributeModelList.add(attributeModel3);

    return attributeModelList;
  }

  @Test
  public void filterDuplicateProducts() throws IOException, SolrServerException {
    SolrDocument solrDocument1 = new SolrDocument();
    solrDocument1.addField("product_code", "product_code");
    solrDocument1.addField("name", "name");
    SolrDocumentList solrDocList = new SolrDocumentList();
    solrDocList.add(solrDocument1);
    Mockito.when(this.queryResponse.getResults()).thenReturn(solrDocList);
    Mockito.when(solrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<AttributeModel> attributeModelList = generateAttributeModelList();
    solrProductModel =
        new SolrProductModel(NAME, UPC_CODE, FINAL_CATEGORY_ID, attributeModelList, null);
    solrProductFilterService.filterDuplicateProducts(solrProductModel, 10);
    Mockito.verify(solrClient).query(solrParamsArgumentCaptor.capture());
    Assertions.assertTrue(solrParamsArgumentCaptor.getValue().toString().contains(NAME_CLEANED_FUZZY));
    Mockito.verify(this.queryResponse).getResults();
  }

  @Test
  public void filterDuplicateProducts_attributeModelListSizeLowerThanTwo() throws Exception {
    SolrDocument solrDocument1 = new SolrDocument();
    solrDocument1.addField("product_code", "product_code");
    solrDocument1.addField("name", "name");
    SolrDocumentList solrDocList = new SolrDocumentList();
    solrDocList.add(solrDocument1);
    Mockito.when(this.queryResponse.getResults()).thenReturn(solrDocList);
    Mockito.when(solrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<AttributeModel> attributeModelList = generateAttributeModelList();
    attributeModelList.remove(2);
    solrProductModel =
        new SolrProductModel(NAME, UPC_CODE, FINAL_CATEGORY_ID, attributeModelList, null);
    solrProductFilterService.filterDuplicateProducts(solrProductModel, 10);
    Mockito.verify(solrClient).query(solrParamsArgumentCaptor.capture());
    Mockito.verify(this.queryResponse).getResults();
    Assertions.assertTrue(solrParamsArgumentCaptor.getValue().toString().contains(NAME_CLEANED_FUZZY));
  }

  @Test
  public void filterDuplicateProducts_responseSizeEqualsTen() throws Exception {
    SolrDocument solrDocument1 = new SolrDocument();
    solrDocument1.addField("product_code", "product_code");
    solrDocument1.addField("name", "name");
    SolrDocumentList solrDocList = new SolrDocumentList();
    for (int i = 1; i < 11; i++) {
      solrDocList.add(solrDocument1);
      solrDocList.set(i - 1, solrDocument1);
    }
    Mockito.when(this.queryResponse.getResults()).thenReturn(solrDocList);
    Mockito.when(solrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<AttributeModel> attributeModelList = generateAttributeModelList();
    solrProductModel =
        new SolrProductModel(NAME, UPC_CODE, FINAL_CATEGORY_ID, attributeModelList, null);
    solrProductFilterService.filterDuplicateProducts(solrProductModel, 10);
    Mockito.verify(solrClient).query(solrParamsArgumentCaptor.capture());
    Mockito.verify(this.queryResponse).getResults();
    Assertions.assertTrue(solrParamsArgumentCaptor.getValue().toString().contains(NAME_CLEANED_FUZZY));
  }

  @Test
  public void filterDuplicateProduct_emptyAttributeModelList() throws Exception {
    SolrDocument solrDocument1 = new SolrDocument();
    solrDocument1.addField("product_code", "product_code");
    solrDocument1.addField("name", "name");
    SolrDocumentList solrDocList = new SolrDocumentList();
    solrDocList.add(solrDocument1);
    Mockito.when(this.queryResponse.getResults()).thenReturn(solrDocList);
    Mockito.when(solrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<AttributeModel> attributeModelList = new ArrayList<AttributeModel>();
    solrProductModel =
        new SolrProductModel(NAME, UPC_CODE, FINAL_CATEGORY_ID, attributeModelList, null);
    solrProductFilterService.filterDuplicateProducts(solrProductModel, 10);
    Mockito.verify(solrClient).query(solrParamsArgumentCaptor.capture());
    Mockito.verify(this.queryResponse).getResults();
    Assertions.assertTrue(solrParamsArgumentCaptor.getValue().toString().contains(NAME_CLEANED_FUZZY));
  }

  @Test
  public void filterDuplicateProducts_nullResponse1() throws Exception {
    QueryResponse queryResponse = new QueryResponse();
    NamedList<Object> res = new NamedList<>();
    res.add("grouped", null);
    queryResponse.setResponse(res);
    Mockito.when(solrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<AttributeModel> attributeModelList = generateAttributeModelList();
    solrProductModel =
        new SolrProductModel(NAME, UPC_CODE, FINAL_CATEGORY_ID, attributeModelList, null);
    solrProductFilterService.filterDuplicateProducts(solrProductModel, 10);
    Mockito.verify(solrClient).query(solrParamsArgumentCaptor.capture());
    Assertions.assertTrue(solrParamsArgumentCaptor.getValue().toString().contains(NAME_CLEANED_FUZZY));
  }

  @Test
  public void filterDuplicateProducts_nullResponse1Values() throws Exception {
    QueryResponse queryResponse = new QueryResponse();
    NamedList<Object> res = new NamedList<>();
    NamedList<Object> res1 = new NamedList<>();
    res1.add("product_code", new SimpleOrderedMap<Object>());
    res.add("grouped", res1);
    queryResponse.setResponse(res);
    Mockito.when(solrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<AttributeModel> attributeModelList = generateAttributeModelList();
    solrProductModel =
        new SolrProductModel(NAME, UPC_CODE, FINAL_CATEGORY_ID, attributeModelList, null);
    solrProductFilterService.filterDuplicateProducts(solrProductModel, 10);
    Mockito.verify(solrClient).query(solrParamsArgumentCaptor.capture());
    Assertions.assertTrue(solrParamsArgumentCaptor.getValue().toString().contains(NAME_CLEANED_FUZZY));
  }

  @Test
  public void filterDuplicateProducts_executeQuery_nullProductNameField() throws Exception {
    SolrDocument solrDocument1 = new SolrDocument();
    solrDocument1.addField("product_code", "product_code");
    solrDocument1.addField("name", null);
    SolrDocumentList solrDocList = new SolrDocumentList();
    solrDocList.add(solrDocument1);
    Mockito.when(this.queryResponse.getResults()).thenReturn(solrDocList);
    Mockito.when(solrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<AttributeModel> attributeModelList = generateAttributeModelList();
    solrProductModel =
        new SolrProductModel(NAME, UPC_CODE, FINAL_CATEGORY_ID, attributeModelList, null);
    solrProductFilterService.filterDuplicateProducts(solrProductModel, 10);
    Mockito.verify(solrClient).query(solrParamsArgumentCaptor.capture());
    Mockito.verify(this.queryResponse).getResults();
    Assertions.assertTrue(solrParamsArgumentCaptor.getValue().toString().contains(NAME_CLEANED_FUZZY));
  }

  @Test
  public void filterDuplicateProducts_executeQuery_commandNameNotEqualsProductCode()
      throws Exception {
    SolrDocument solrDocument1 = new SolrDocument();
    solrDocument1.addField("product_code", "product_code");
    solrDocument1.addField("name", "name");
    SolrDocumentList solrDocList = new SolrDocumentList();
    solrDocList.add(solrDocument1);
    Mockito.when(this.queryResponse.getResults()).thenReturn(solrDocList);
    Mockito.when(solrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<AttributeModel> attributeModelList = generateAttributeModelList();
    solrProductModel =
        new SolrProductModel(NAME, UPC_CODE, FINAL_CATEGORY_ID, attributeModelList, null);
    solrProductFilterService.filterDuplicateProducts(solrProductModel, 10);
    Mockito.verify(solrClient).query(solrParamsArgumentCaptor.capture());
    Mockito.verify(this.queryResponse).getResults();
    Assertions.assertTrue(solrParamsArgumentCaptor.getValue().toString().contains(NAME_CLEANED_FUZZY));
  }

  @Test
  public void filterDuplicateProducts_executeQuery_exception() throws Exception {
    SolrDocument solrDocument1 = new SolrDocument();
    solrDocument1.addField("name", "name");
    SolrDocumentList solrDocList = new SolrDocumentList();
    solrDocList.add(solrDocument1);
    Mockito.when(this.queryResponse.getResults()).thenReturn(solrDocList);
    Mockito.when(solrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<AttributeModel> attributeModelList = generateAttributeModelList();
    solrProductModel =
        new SolrProductModel(NAME, UPC_CODE, FINAL_CATEGORY_ID, attributeModelList, null);
    solrProductFilterService.filterDuplicateProducts(solrProductModel, 10);
    Mockito.verify(solrClient).query(solrParamsArgumentCaptor.capture());
    Mockito.verify(this.queryResponse).getResults();
    Assertions.assertTrue(solrParamsArgumentCaptor.getValue().toString().contains(NAME_CLEANED_FUZZY));
  }

  @Test
  public void getActiveProductIds() throws Exception {
    List<AttributeModel> attributeModelList = generateAttributeModelList();
    solrProductModel = new SolrProductModel(NAME, UPC_CODE, FINAL_CATEGORY_ID, attributeModelList, new Date());
    solrProductModel.setProductCategoryId(FINAL_CATEGORY_ID);
    SolrDocument solrDocument1 = new SolrDocument();
    solrDocument1.addField("product_code", "product_code");
    solrDocument1.addField("name", "name");
    SolrDocumentList solrDocList = new SolrDocumentList();
    solrDocList.setNumFound(1);
    solrDocList.add(solrDocument1);

    Mockito.when(this.queryResponse.getResults()).thenReturn(solrDocList);
    Mockito.when(solrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);

    solrProductFilterService.getActiveProductIds(solrProductModel, 0, 2);

    Mockito.verify(this.queryResponse, Mockito.times(2)).getResults();
    Mockito.verify(solrClient).query(solrParamsArgumentCaptor.capture());
    Assertions.assertEquals(CATEGORY_ID_FIELD + DOUBLE_QUOTES_FOR_SOLR_QUERY + FINAL_CATEGORY_ID + DOUBLE_QUOTES_FOR_SOLR_QUERY, solrParamsArgumentCaptor.getValue().get("fq"));
    Assertions.assertEquals("1", solrParamsArgumentCaptor.getValue().get("start"));
    Assertions.assertEquals("2", solrParamsArgumentCaptor.getValue().get("rows"));
  }

  @Test
  public void getActiveProductIds_nullUpdatedDate() throws Exception {
    List<AttributeModel> attributeModelList = generateAttributeModelList();
    solrProductModel =
        new SolrProductModel(NAME, UPC_CODE, FINAL_CATEGORY_ID, attributeModelList, null);
    SolrDocument solrDocument1 = new SolrDocument();
    solrDocument1.addField("product_code", "product_code");
    solrDocument1.addField("name", "name");
    SolrDocumentList solrDocList = new SolrDocumentList();
    solrDocList.setNumFound(1);
    solrDocList.add(solrDocument1);

    Mockito.when(this.queryResponse.getResults()).thenReturn(solrDocList);
    Mockito.when(solrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);

    solrProductFilterService.getActiveProductIds(solrProductModel, 0, 10);

    Mockito.verify(solrClient).query(solrParamsArgumentCaptor.capture());
    Mockito.verify(this.queryResponse, Mockito.times(2)).getResults();
  }

  @Test
  public void getActiveProductIds_whenSolrFails() throws Exception {
    Mockito.when(solrClient.query(Mockito.any(SolrQuery.class))).thenThrow(RuntimeException.class);

    solrProductModel = new SolrProductModel(NAME, UPC_CODE, FINAL_CATEGORY_ID, Collections.emptyList(), null);

    solrProductFilterService.getActiveProductIds(solrProductModel, 0, 10);

    Mockito.verify(solrClient).query(Mockito.any(SolrQuery.class));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(solrClient, queryResponse);
  }
}
