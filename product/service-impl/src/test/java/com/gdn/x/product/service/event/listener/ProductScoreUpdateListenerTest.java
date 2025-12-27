package com.gdn.x.product.service.event.listener;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.productcategorybase.domain.event.model.ProductScoreUpdateDomainEventModel;

public class ProductScoreUpdateListenerTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "10001";
  private static final String MANDATORY_ATTRIBUTE_SCORE = "mandatoryAttributeScore";
  private static final String USP_SCORE = "uspScore";
  private static final String MESSAGE = "message";
  private Map<String, Double> scoreMap;
  private List<ProductAndItemsVO> productAndItemsVOList = new ArrayList<>();

  @Mock
  private ProductService productService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private ProductScoreUpdateListener productScoreUpdateListener;

  private ProductScoreUpdateDomainEventModel productScoreUpdateDomainEventModel;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    productScoreUpdateDomainEventModel =
        ProductScoreUpdateDomainEventModel.builder().productCodes(Arrays.asList(PRODUCT_CODE)).build();
    scoreMap = new HashMap<>();
    scoreMap.put(MANDATORY_ATTRIBUTE_SCORE, 12.5);
    scoreMap.put(USP_SCORE, 5.0);
    productAndItemsVOList.add(new ProductAndItemsVO());
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.productService.updateProductScoreOnMasterDataChange(STORE_ID, PRODUCT_CODE, true, null, false,
            false, null, null))
        .thenReturn(productAndItemsVOList);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductScoreUpdateDomainEventModel.class))
      .thenReturn(productScoreUpdateDomainEventModel);
    this.productScoreUpdateListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductScoreUpdateDomainEventModel.class);
    Mockito.verify(this.productService).updateProductScoreOnMasterDataChange(STORE_ID, PRODUCT_CODE, true, null, false,
        false, null, null);
    verify(this.productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(null, new ArrayList<>(), true);
  }

  @Test
  public void onDomainEventConsumedEmptyTest() throws Exception {
    productScoreUpdateDomainEventModel.setProductCodes(new ArrayList<>());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductScoreUpdateDomainEventModel.class))
      .thenReturn(productScoreUpdateDomainEventModel);
    this.productScoreUpdateListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductScoreUpdateDomainEventModel.class);
    Mockito.verify(this.productService, Mockito.times(0))
        .updateProductScoreOnMasterDataChange(STORE_ID, PRODUCT_CODE, true, null, false, false, null, null);
    verify(this.productAndItemSolrIndexerService, Mockito.times(0))
        .updateProductDetailsInSolr(null);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductScoreUpdateDomainEventModel.class))
      .thenReturn(productScoreUpdateDomainEventModel);
    doThrow(ApplicationRuntimeException.class).when(this.productService)
        .updateProductScoreOnMasterDataChange(STORE_ID, PRODUCT_CODE, true, null, false, false, null, null);
    try {
      this.productScoreUpdateListener.onDomainEventConsumed(MESSAGE);
    } finally {
      Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductScoreUpdateDomainEventModel.class);
      Mockito.verify(this.productService).updateProductScoreOnMasterDataChange(STORE_ID, PRODUCT_CODE, true, null, false,
          false, null, null);
    }
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.productService);
  }
}