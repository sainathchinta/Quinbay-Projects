package com.gdn.x.productcategorybase.domainevent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.entity.solr.SolrPcbProductModel;
import com.gdn.x.productcategorybase.repository.SolrPcbRepository;

public class SolrAddPcbProductSubscriberTest {

  @Mock
  private SolrPcbRepository solrPcbRepository;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private SolrAddPcbProductSubscriber solrAddPcbProductSubscriber;

  @Captor
  private ArgumentCaptor<List<SolrPcbProductModel>> solrPcbProductModelsArgumentCaptor;

  private SolrAddBatchPcbProductDomainEventModel solrAddBatchPcbProductDomainEventModel;

  private static final String ID = "ID";
  private static final String NAME = "name";
  private static final String MESSAGE = "message";
  private static final String CATEGORY_ID = "categoryId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PARENT_CATEGORY_ID = "parentCategoryId";
  private static final List<String> UPC_CODES = Arrays.asList("upcCode1", "upcCode2");
  private static final List<String> SKU_CODES = Arrays.asList("skuCode1", "skuCode2");
  private static final List<String> GENERATED_ITEM_NAMES = Arrays.asList("itemName1", "itemName2");
  private static final List<Integer> DANGEROUS_GOODS_LEVELS = Arrays.asList(0, 1);
  private static final List<String> IMAGE_LOCATION_PATHS = Arrays.asList("imagePath1", "imagePath2");

  private String message;

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
    solrAddBatchPcbProductDomainEventModel = new SolrAddBatchPcbProductDomainEventModel();
    SolrAddPcbProductDomainEventModel solrAddPcbProductDomainEventModel =
        SolrAddPcbProductDomainEventModel.builder().id(ID).name(NAME).categoryId(CATEGORY_ID).productCode(PRODUCT_CODE)
            .parentCategoryId(PARENT_CATEGORY_ID).upcCodes(UPC_CODES).skuCodes(SKU_CODES)
            .generatedItemNames(GENERATED_ITEM_NAMES).dangerousGoodsLevels(DANGEROUS_GOODS_LEVELS)
            .imageLocationPaths(IMAGE_LOCATION_PATHS).build();
    ObjectMapper mapper = new ObjectMapper();
    message = mapper.writeValueAsString(solrAddBatchPcbProductDomainEventModel);
    Mockito.when(objectMapper.readValue(message, SolrAddBatchPcbProductDomainEventModel.class))
        .thenReturn(solrAddBatchPcbProductDomainEventModel);
    solrAddBatchPcbProductDomainEventModel
        .setProductDomainEventModelList(Arrays.asList(solrAddPcbProductDomainEventModel));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(solrPcbRepository, objectMapper);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    when(objectMapper.readValue(MESSAGE, SolrAddBatchPcbProductDomainEventModel.class))
        .thenReturn(solrAddBatchPcbProductDomainEventModel);
    solrAddPcbProductSubscriber.onDomainEventConsumed(MESSAGE);
    verify(solrPcbRepository).addProductListToPcbCollection(solrPcbProductModelsArgumentCaptor.capture());
    SolrPcbProductModel solrPcbProductModel = solrPcbProductModelsArgumentCaptor.getValue().get(0);
    assertEquals(ID, solrPcbProductModel.getId());
    assertEquals(NAME, solrPcbProductModel.getName());
    assertEquals(CATEGORY_ID, solrPcbProductModel.getCategoryId());
    assertEquals(PRODUCT_CODE, solrPcbProductModel.getProductCode());
    assertEquals(PARENT_CATEGORY_ID, solrPcbProductModel.getParentCategoryId());
    assertEquals(UPC_CODES, solrPcbProductModel.getUpcCodes());
    assertEquals(SKU_CODES, solrPcbProductModel.getSkuCodes());
    assertEquals(GENERATED_ITEM_NAMES, solrPcbProductModel.getGeneratedItemNames());
    assertEquals(DANGEROUS_GOODS_LEVELS, solrPcbProductModel.getDangerousGoodsLevels());
    assertEquals(IMAGE_LOCATION_PATHS, solrPcbProductModel.getImageLocationPaths());
    verify(objectMapper).readValue(MESSAGE, SolrAddBatchPcbProductDomainEventModel.class);
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    doThrow(RuntimeException.class).when(objectMapper).readValue(MESSAGE, SolrAddBatchPcbProductDomainEventModel.class);
    solrAddPcbProductSubscriber.onDomainEventConsumed(MESSAGE);
    verify(objectMapper).readValue(MESSAGE, SolrAddBatchPcbProductDomainEventModel.class);
  }
}