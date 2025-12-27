package com.gdn.partners.product.orchestrator.service.product.level1;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.repository.ProductBusinessPartnerCustomRepository;
import com.gdn.mta.product.repository.ProductHistoryRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.partners.pbp.model.vo.ProductLv1IdxLv3IdVO;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import com.gdn.partners.product.orchestrator.dto.product.level1.ProductLevel1FilterResponse;
import com.gdn.partners.product.orchestrator.entity.product.level1.ProductLevel1;
import com.gdn.partners.product.orchestrator.model.product.level1.ProductLevel1Filter;
import com.gdn.partners.product.orchestrator.repository.data.product.level1.ProductLevel1Repository;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public class ProductLevel1ServiceTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_REQUEST_ID = "test";
  private static final String DEFAULT_USERNAME = "test";
  private static final String DEFAULT_CODE = "CODE";
  private static final String DEFAULT_PRODUCT_ID = "PRODUCT-ID";
  private static final String DEFAULT_REVIEWER_NOTE = "REVIEWER_NOTE";
  private static final String DEFAULT_STATE = "STATE";
  private static final String DEFAULT_PRODUCT_SKU = "PRODUCT-SKU";
  private static final String DEFAULT_MERCHANT_SKU = "MERCHANT-SKU";
  private static final Pageable DEFAULT_PAGEABLE = PageRequest.of(0, 1);
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);

  @Mock
  private ProductLevel1Repository productLevel1Repository;

  @Mock
  private ProductBusinessPartnerCustomRepository productBusinessPartnerCustomRepository;

  @Mock
  private ProductRepository productLevel1Outbound;

  @Mock
  private ProductWorkflowRepository productWorkflowRepository;

  @Mock
  private ProductHistoryRepository productHistoryRepository;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @InjectMocks
  private ProductLevel1ServiceBean productLevel1ServiceBean;

  private Page<ProductLevel1> generateProductLevel1s() {
    return new PageImpl<>(
        Collections.singletonList(ProductLevel1.builder()
            .productId(ProductLevel1ServiceTest.DEFAULT_PRODUCT_ID)
            .code(ProductLevel1ServiceTest.DEFAULT_CODE)
            .state(ProductLevel1State.ACTIVE)
            .build()));
  }

  private Page<ProductLevel1> generateProductLevel1s_1() {
    ProductLevel1 productLevel1 =
        ProductLevel1.builder()
            .productId(ProductLevel1ServiceTest.DEFAULT_PRODUCT_ID)
            .code(ProductLevel1ServiceTest.DEFAULT_CODE)
            .state("DELETED")
            .reviewerNotes(DEFAULT_REVIEWER_NOTE)
            .build();
    productLevel1.setStoreId(DEFAULT_STORE_ID);
    return new PageImpl<>(Collections.singletonList(productLevel1));
  }

  private Page<ProductLevel1> generateProductLevel1s_2() {
    ProductLevel1 productLevel1 =
        ProductLevel1.builder().productId(ProductLevel1ServiceTest.DEFAULT_PRODUCT_ID)
            .code(ProductLevel1ServiceTest.DEFAULT_CODE).state(ProductLevel1State.NEED_CORRECTION)
            .reviewerNotes(DEFAULT_REVIEWER_NOTE).build();
    productLevel1.setStoreId(DEFAULT_STORE_ID);
    return new PageImpl<>(Collections.singletonList(productLevel1));
  }

  private Page<ProductLevel1> generateProductLevel1s_3() {
    ProductLevel1 productLevel1 =
        ProductLevel1.builder().productId(ProductLevel1ServiceTest.DEFAULT_PRODUCT_ID)
            .code(ProductLevel1ServiceTest.DEFAULT_CODE).state(null)
            .reviewerNotes(DEFAULT_REVIEWER_NOTE).build();
    productLevel1.setStoreId(DEFAULT_STORE_ID);
    return new PageImpl<>(Collections.singletonList(productLevel1));
  }

  private GdnRestListResponse<ProductDetailResponse> generateMasterProducts() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setId(DEFAULT_PRODUCT_ID);
    productDetailResponse.setProductCode(ProductLevel1ServiceTest.DEFAULT_CODE);
    return new GdnRestListResponse<>(Collections.singletonList(productDetailResponse),
        new PageMetaData(1, 0, 1), null);
  }

  private ProductLv1IdxLv3IdVO generateProductSkuInfoVo() {
    ProductLv1IdxLv3IdVO productLv1IdxLv3IdVO = new ProductLv1IdxLv3IdVO();
    productLv1IdxLv3IdVO.setProductId(ProductLevel1ServiceTest.DEFAULT_PRODUCT_ID);
    productLv1IdxLv3IdVO.setGdnProductSku(ProductLevel1ServiceTest.DEFAULT_PRODUCT_SKU);
    return productLv1IdxLv3IdVO;
  }

  @BeforeEach
  public void setup() throws Exception {
    ProductSystemParameter productSystemParameter2 = new ProductSystemParameter();
    productSystemParameter2.setValue("10");
    MockitoAnnotations.initMocks(this);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        ProductLevel1ServiceTest.DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        ProductLevel1ServiceTest.DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        ProductLevel1ServiceTest.DEFAULT_USERNAME);
    Mockito.when(this.productLevel1Repository
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(this.generateProductLevel1s());
    Mockito.when(this.productBusinessPartnerCustomRepository.findProductLv1IdxLv3IdVO(
        Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(Collections.singletonList(generateProductSkuInfoVo()));
    Mockito.when(this.productLevel1Outbound
        .getAllProductDetailsByProductCodes(Mockito.any(), Mockito.any(),
            Mockito.any())).thenReturn(this.generateMasterProducts());

    Mockito.when(productSystemParameterService
      .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.PRODUCT_FETCH_BATCH_SIZE))
      .thenReturn(productSystemParameter2);
  }

  @AfterEach
  public void teardown() {
    MDC.remove(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    MDC.remove(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    MDC.remove(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    Mockito.verifyNoMoreInteractions(this.productLevel1Repository);
    Mockito.verifyNoMoreInteractions(this.productBusinessPartnerCustomRepository);
    Mockito.verifyNoMoreInteractions(this.productLevel1Outbound);
  }

  @Test
  public void findByFilter_Valid_Success() throws Exception {
    Page<ProductLevel1FilterResponse> result =
        this.productLevel1ServiceBean.findByFilter(ProductLevel1Filter.builder().build(),
            ProductLevel1ServiceTest.DEFAULT_PAGEABLE);
    Mockito.when(
        productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_ID))
        .thenReturn(new ArrayList<>());
    Assertions.assertFalse(result.getContent().isEmpty());
    result.getContent()
        .forEach(response ->
            Assertions.assertEquals(ProductLevel1ServiceTest.DEFAULT_PRODUCT_SKU,
                response.getProductSku()));
    Mockito.verify(this.productLevel1Repository).findByStoreIdAndFilterAndMarkForDeleteFalse(
        Mockito.eq(ProductLevel1ServiceTest.DEFAULT_STORE_ID),
        Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productLevel1Outbound)
        .getAllProductDetailsByProductCodes(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_REQUEST_ID),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_USERNAME),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_CODE)));
    Mockito.verify(this.productBusinessPartnerCustomRepository)
        .findProductLv1IdxLv3IdVO(Mockito.eq(null), Mockito.eq(null),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_PRODUCT_ID)));
  }

  @Test
  public void findByFilterRejectionNotesNeedCorrection() throws Exception {
    Page<ProductLevel1> productLevel1s = this.generateProductLevel1s();
    productLevel1s.getContent().get(0).setState("NEED_CORRECTION");
    Mockito.when(this.productLevel1Repository
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(productLevel1s);
    ProductLv1IdxLv3IdVO productLv1IdxLv3IdVO = generateProductSkuInfoVo();
    productLv1IdxLv3IdVO.setPreOrder(true);
    Mockito.when(this.productBusinessPartnerCustomRepository.findProductLv1IdxLv3IdVO(
        Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(Collections.singletonList(productLv1IdxLv3IdVO));
    Mockito.when(productHistoryRepository.findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(
        DEFAULT_STORE_ID, DEFAULT_PRODUCT_ID,
        WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc())).thenReturn(new ProductHistory());
    Mockito.when(productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_ID)).thenReturn(new ArrayList<>());
    Page<ProductLevel1FilterResponse> result =
        this.productLevel1ServiceBean.findByFilter(ProductLevel1Filter.builder().build(),
            ProductLevel1ServiceTest.DEFAULT_PAGEABLE);
    Assertions.assertFalse(result.getContent().isEmpty());
    result.getContent()
        .forEach(response ->
            Assertions.assertEquals(ProductLevel1ServiceTest.DEFAULT_PRODUCT_SKU,
                response.getProductSku()));
    result.getContent().forEach(response -> Assertions.assertTrue(response.isPreOrder()));
    Mockito.verify(this.productLevel1Repository).findByStoreIdAndFilterAndMarkForDeleteFalse(
        Mockito.eq(ProductLevel1ServiceTest.DEFAULT_STORE_ID), Mockito.any(), Mockito.any());
    Mockito.verify(this.productLevel1Outbound)
        .getAllProductDetailsByProductCodes(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_REQUEST_ID),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_USERNAME),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_CODE)));
    Mockito.verify(this.productBusinessPartnerCustomRepository)
        .findProductLv1IdxLv3IdVO(Mockito.eq(null), Mockito.eq(null),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_PRODUCT_ID)));
  }


  @Test
  public void findByFilterRejectionNotes() throws Exception {
    Page<ProductLevel1> productLevel1s = this.generateProductLevel1s();
    productLevel1s.getContent().get(0).setState("DELETED");
    Mockito.when(this.productLevel1Repository
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class)))
        .thenReturn(productLevel1s);
    Mockito.when(
        productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_ID))
        .thenReturn(new ArrayList<>());
    Page<ProductLevel1FilterResponse> result =
        this.productLevel1ServiceBean.findByFilter(ProductLevel1Filter.builder().build(),
            ProductLevel1ServiceTest.DEFAULT_PAGEABLE);
    Assertions.assertFalse(result.getContent().isEmpty());
    result.getContent()
        .forEach(response ->
            Assertions.assertEquals(ProductLevel1ServiceTest.DEFAULT_PRODUCT_SKU,
                response.getProductSku()));
    result.getContent().forEach(response -> Assertions.assertFalse(response.isPreOrder()));
    Mockito.verify(this.productLevel1Repository).findByStoreIdAndFilterAndMarkForDeleteFalse(
        Mockito.eq(ProductLevel1ServiceTest.DEFAULT_STORE_ID),
        Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productLevel1Outbound)
        .getAllProductDetailsByProductCodes(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_REQUEST_ID),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_USERNAME),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_CODE)));
    Mockito.verify(this.productBusinessPartnerCustomRepository)
        .findProductLv1IdxLv3IdVO(Mockito.eq(null), Mockito.eq(null),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_PRODUCT_ID)));
  }

  @Test
  public void findByFilter_Null() throws Exception {
    Mockito.when(this.productBusinessPartnerCustomRepository.findProductLv1IdxLv3IdVO(
        Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(Collections.singletonList(new ProductLv1IdxLv3IdVO()));
    Page<ProductLevel1FilterResponse> result =
        this.productLevel1ServiceBean.findByFilter(ProductLevel1Filter.builder().build(),
            ProductLevel1ServiceTest.DEFAULT_PAGEABLE);
    Mockito.when(
        productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_ID))
        .thenReturn(new ArrayList<>());
    Assertions.assertFalse(result.getContent().isEmpty());
    result.getContent()
        .forEach(response ->
            Assertions.assertEquals(null,
                response.getProductSku()));
    Mockito.verify(this.productLevel1Repository).findByStoreIdAndFilterAndMarkForDeleteFalse(
        Mockito.eq(ProductLevel1ServiceTest.DEFAULT_STORE_ID), Mockito.any(), Mockito.any());
    Mockito.verify(this.productLevel1Outbound)
        .getAllProductDetailsByProductCodes(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_REQUEST_ID),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_USERNAME),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_CODE)));
    Mockito.verify(this.productBusinessPartnerCustomRepository)
        .findProductLv1IdxLv3IdVO(Mockito.eq(null), Mockito.eq(null),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_PRODUCT_ID)));
  }

  @Test
  public void findByFilter_StateAndNote_Success() throws Exception {
    List<ProductWorkflow> productWorkflows = new ArrayList<>();
    ProductWorkflow productWorkflow = new ProductWorkflow();
    productWorkflow.setProductId(DEFAULT_PRODUCT_ID);
    productWorkflow.setState(4);
    productWorkflow.setStoreId(DEFAULT_STORE_ID);
    productWorkflow.setNotes(DEFAULT_REVIEWER_NOTE);
    productWorkflows.add(productWorkflow);
    Mockito.when(this.productLevel1Repository
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.any(ProductLevel1Filter.class),
            Mockito.any(Pageable.class))).thenReturn(this.generateProductLevel1s_1());
    Mockito.when(
        productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_ID))
        .thenReturn(productWorkflows);
    ProductLv1IdxLv3IdVO productLv1IdxLv3IdVO = generateProductSkuInfoVo();
    productLv1IdxLv3IdVO.setPreOrder(false);
    Mockito.when(this.productBusinessPartnerCustomRepository.findProductLv1IdxLv3IdVO(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(productLv1IdxLv3IdVO));

    Page<ProductLevel1FilterResponse> result = this.productLevel1ServiceBean
        .findByFilter(ProductLevel1Filter.builder()
                .states(new HashSet<>(Arrays.asList("DELETED")))
                .build(),
            ProductLevel1ServiceTest.DEFAULT_PAGEABLE);
    Assertions.assertFalse(result.getContent().isEmpty());
    result.getContent().forEach(response -> Assertions.assertFalse(response.isPreOrder()));
    result.getContent().forEach(
        response -> Assertions.assertEquals(ProductLevel1ServiceTest.DEFAULT_PRODUCT_SKU,
            response.getProductSku()));
    Mockito.verify(this.productLevel1Repository)
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_STORE_ID),
            Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productLevel1Outbound)
        .getAllProductDetailsByProductCodes(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_REQUEST_ID),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_USERNAME),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_CODE)));
    Mockito.verify(this.productBusinessPartnerCustomRepository)
        .findProductLv1IdxLv3IdVO(Mockito.eq(null), Mockito.eq(null),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_PRODUCT_ID)));
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_PRODUCT_ID);
  }

  @Test
  public void findByFilter_StateAndNote_RevisionNoteSuccess() throws Exception {
    List<ProductWorkflow> productWorkflows = new ArrayList<>();
    ProductWorkflow productWorkflow = new ProductWorkflow();
    ProductHistory productHistory = new ProductHistory();
    productHistory.setNotes("RevisionNotes");
    productWorkflow.setProductId(DEFAULT_PRODUCT_ID);
    productWorkflow.setState(4);
    productWorkflow.setStoreId(DEFAULT_STORE_ID);
    productWorkflow.setNotes(DEFAULT_REVIEWER_NOTE);
    productWorkflows.add(productWorkflow);
    Mockito.when(this.productLevel1Repository
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.any(ProductLevel1Filter.class),
            Mockito.any(Pageable.class))).thenReturn(this.generateProductLevel1s_2());
    Mockito.when(productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(
            DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_ID,
            WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc())).thenReturn(productHistory);
    Page<ProductLevel1FilterResponse> result = this.productLevel1ServiceBean.findByFilter(
        ProductLevel1Filter.builder()
            .states(new HashSet<>(Arrays.asList(ProductLevel1State.NEED_CORRECTION)))
            .build(),
        ProductLevel1ServiceTest.DEFAULT_PAGEABLE);
    Assertions.assertFalse(result.getContent().isEmpty());
    result.getContent().forEach(
        response -> Assertions.assertEquals(ProductLevel1ServiceTest.DEFAULT_PRODUCT_SKU,
            response.getProductSku()));
    Mockito.verify(this.productLevel1Repository)
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_STORE_ID),
            Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productLevel1Outbound)
        .getAllProductDetailsByProductCodes(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_REQUEST_ID),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_USERNAME),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_CODE)));
    Mockito.verify(this.productBusinessPartnerCustomRepository)
        .findProductLv1IdxLv3IdVO(Mockito.eq(null), Mockito.eq(null),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_PRODUCT_ID)));
    Mockito.verify(productHistoryRepository)
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(
            DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_ID,
            WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc());
  }

  @Test
  public void findByFilter_StateNullTest() throws Exception {
    List<ProductWorkflow> productWorkflows = new ArrayList<>();
    ProductWorkflow productWorkflow = new ProductWorkflow();
    ProductHistory productHistory = new ProductHistory();
    productHistory.setNotes("RevisionNotes");
    productWorkflow.setProductId(DEFAULT_PRODUCT_ID);
    productWorkflow.setState(4);
    productWorkflow.setStoreId(DEFAULT_STORE_ID);
    productWorkflow.setNotes(DEFAULT_REVIEWER_NOTE);
    productWorkflows.add(productWorkflow);
    Mockito.when(this.productLevel1Repository
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.any(ProductLevel1Filter.class),
            Mockito.any(Pageable.class))).thenReturn(this.generateProductLevel1s_3());
    Mockito.when(productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(
            DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_ID,
            WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc())).thenReturn(productHistory);
    Page<ProductLevel1FilterResponse> result = this.productLevel1ServiceBean.findByFilter(
        ProductLevel1Filter.builder()
            .states(new HashSet<>(Arrays.asList(ProductLevel1State.NEED_CORRECTION)))
            .build(),
        ProductLevel1ServiceTest.DEFAULT_PAGEABLE);
    Assertions.assertFalse(result.getContent().isEmpty());
    result.getContent().forEach(
        response -> Assertions.assertEquals(ProductLevel1ServiceTest.DEFAULT_PRODUCT_SKU,
            response.getProductSku()));
    Mockito.verify(this.productLevel1Repository)
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_STORE_ID),
            Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productLevel1Outbound)
        .getAllProductDetailsByProductCodes(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_REQUEST_ID),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_USERNAME),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_CODE)));
    Mockito.verify(this.productBusinessPartnerCustomRepository)
        .findProductLv1IdxLv3IdVO(Mockito.eq(null), Mockito.eq(null),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_PRODUCT_ID)));
  }

  @Test
  public void findByFilter_FilterMerchantSkuNotNull_NotFound_Success() throws Exception {
    Mockito.when(this.productBusinessPartnerCustomRepository.findProductLv1IdxLv3IdVO(
        Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(Collections.emptyList());
    Page<ProductLevel1FilterResponse> result = this.productLevel1ServiceBean.findByFilter(
        ProductLevel1Filter.builder().merchantSku(ProductLevel1ServiceTest.DEFAULT_MERCHANT_SKU)
            .build(), ProductLevel1ServiceTest.DEFAULT_PAGEABLE);
    Mockito.when(
        productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_ID))
        .thenReturn(new ArrayList<>());
    System.out.println(result.getContent().toString());
    Assertions.assertTrue(result.getContent().isEmpty());
    Mockito.verify(this.productBusinessPartnerCustomRepository)
        .findProductLv1IdxLv3IdVO(Mockito.eq(null),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_MERCHANT_SKU), Mockito.eq(null));
  }

  @Test
  public void findByFilter_FilterMerchantSkuNotNull_Success() throws Exception {
    Page<ProductLevel1FilterResponse> result = this.productLevel1ServiceBean.findByFilter(
        ProductLevel1Filter.builder().merchantSku(ProductLevel1ServiceTest.DEFAULT_MERCHANT_SKU)
            .build(), ProductLevel1ServiceTest.DEFAULT_PAGEABLE);
    Mockito.when(
        productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_ID))
        .thenReturn(new ArrayList<>());
    Assertions.assertFalse(result.getContent().isEmpty());
    result.getContent()
        .forEach(response ->
            Assertions.assertEquals(ProductLevel1ServiceTest.DEFAULT_PRODUCT_SKU,
                response.getProductSku()));
    result.getContent()
        .forEach(response ->
            Assertions.assertFalse(response.isPreOrder()));
    Mockito.verify(this.productLevel1Repository).findByStoreIdAndFilterAndMarkForDeleteFalse(
        Mockito.eq(ProductLevel1ServiceTest.DEFAULT_STORE_ID),
        Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productBusinessPartnerCustomRepository)
        .findProductLv1IdxLv3IdVO(Mockito.eq(null),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_MERCHANT_SKU), Mockito.eq(null));
    Mockito.verify(this.productLevel1Outbound)
        .getAllProductDetailsByProductCodes(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_REQUEST_ID),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_USERNAME), Mockito.anyList());
  }

  @Test
  public void findByFilter_FilterMerchantSkuNotNullAndProductIdsIsEmptyAndCodesIsEmpty_Success()
      throws Exception {
    Mockito.when(this.productBusinessPartnerCustomRepository
        .findProductLv1IdxLv3IdVO(Mockito.eq(null),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_MERCHANT_SKU), Mockito.eq(null)))
        .thenReturn(Collections.emptyList());
    Page<ProductLevel1FilterResponse> result = this.productLevel1ServiceBean.findByFilter(
        ProductLevel1Filter.builder().merchantSku(ProductLevel1ServiceTest.DEFAULT_MERCHANT_SKU)
            .build(), ProductLevel1ServiceTest.DEFAULT_PAGEABLE);
    Assertions.assertTrue(result.getContent().isEmpty());
    Mockito.verify(this.productBusinessPartnerCustomRepository)
        .findProductLv1IdxLv3IdVO(Mockito.eq(null),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_MERCHANT_SKU), Mockito.eq(null));
  }

  @Test
  public void findByFilter_MapResponseNull_Success()
      throws Exception {
    GdnRestListResponse<ProductDetailResponse> productDetailResponses = generateMasterProducts();
    productDetailResponses.getContent().get(0).setProductCode("ProductCodeNew");
    Mockito.when(this.productLevel1Repository
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class)))
        .thenReturn(generateProductLevel1s());
    Mockito.when(this.productLevel1Outbound
        .getAllProductDetailsByProductCodes(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyList())).thenReturn(productDetailResponses);
    Page<ProductLevel1FilterResponse> result = this.productLevel1ServiceBean.findByFilter(
        ProductLevel1Filter.builder().merchantSku(ProductLevel1ServiceTest.DEFAULT_MERCHANT_SKU)
            .build(), ProductLevel1ServiceTest.DEFAULT_PAGEABLE);
    Mockito.verify(this.productLevel1Repository)
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productBusinessPartnerCustomRepository)
        .findProductLv1IdxLv3IdVO(Mockito.eq(null),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_MERCHANT_SKU), Mockito.eq(null));
    Mockito.verify(this.productLevel1Outbound)
        .getAllProductDetailsByProductCodes(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_REQUEST_ID),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_USERNAME),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_CODE)));
    Assertions.assertTrue(result.getContent().isEmpty());
  }

  @Test
  public void findByFilter_PostLiveTrue_Success()
      throws Exception {
    Page<ProductLevel1> productLevel1s = generateProductLevel1s();
    productLevel1s.getContent().get(0).setPostLive(true);
    Mockito.when(this.productLevel1Repository
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class)))
        .thenReturn(productLevel1s);
    Mockito.when(this.productLevel1Outbound
        .getAllProductDetailsByProductCodes(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyList())).thenReturn(generateMasterProducts());
    Page<ProductLevel1FilterResponse> result = this.productLevel1ServiceBean.findByFilter(
        ProductLevel1Filter.builder().merchantSku(ProductLevel1ServiceTest.DEFAULT_MERCHANT_SKU)
            .build(), ProductLevel1ServiceTest.DEFAULT_PAGEABLE);
    Mockito.verify(this.productLevel1Repository)
        .findByStoreIdAndFilterAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productBusinessPartnerCustomRepository)
        .findProductLv1IdxLv3IdVO(Mockito.eq(null),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_MERCHANT_SKU), Mockito.eq(null));
    Mockito.verify(this.productLevel1Outbound)
        .getAllProductDetailsByProductCodes(Mockito.eq(ProductLevel1ServiceTest.DEFAULT_REQUEST_ID),
            Mockito.eq(ProductLevel1ServiceTest.DEFAULT_USERNAME),
            Mockito.eq(Collections.singletonList(ProductLevel1ServiceTest.DEFAULT_CODE)));
    Assertions.assertTrue(result.getContent().get(0).isPostLive());
  }
}
