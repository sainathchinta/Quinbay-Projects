package com.gdn.partners.pdt.service.distribution;

import static org.mockito.ArgumentMatchers.eq;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gdn.mta.product.commons.constant.RestrictedKeywordFieldNames;
import com.gdn.x.mta.distributiontask.dao.api.ProductReviewerRepository;
import com.gdn.x.mta.distributiontask.dao.api.feign.PCBFeign;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import org.apache.commons.lang3.StringUtils;
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
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ImageQcProcessedResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pdt.repository.configuration.distribution.AutoDistributionConfigurationRepository;
import com.gdn.partners.pdt.repository.sequence.SequenceRepository;
import com.gdn.partners.pdt.service.product.ProductService;
import com.gdn.x.mta.distributiontask.dao.api.ProductDistributionTaskRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.VendorRepository;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public class DistributionTaskServiceTest {

  private static final String STORE_ID = "storeId";
  private static final String VENDOR_ID = "vendorId";
  private static final String STATE_IN_REVIW = "IN_REVIEW";
  private static final String PRODUCT_CODE = "productCode";
  private static final String REQUEST_ID  = "id";
  private static final String USERNAME  = "name";
  private static final String RESTRICTED_KEYWORD = "restrictedKeyword";
  private static final Boolean AUTO_DISTRIBUTE_SWITCH = Boolean.TRUE;
  private ImageQcProcessedResponse imageQcProcessedResponse;
  private ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse;
  private Product vendorProduct;
  private String restrictedKeywordFieldJson;
  private ObjectMapper mapper;

  private static final VerificationMode NEVER_CALLED = Mockito.times(0);

  @Mock
  private AutoDistributionConfigurationRepository autoDistributionConfigurationRepository;

  @Mock
  private VendorRepository vendorRepository;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductService productService;

  @Mock
  private ProductDistributionTaskRepository distributionTaskRepository;

  @Mock
  private DistributionTaskHistoryService distributionTaskHistoryService;

  @Mock
  private SequenceRepository sequenceRepository;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private ApprovedProductPublisherService approvedProductPublisherService;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private SolrVendorCollectionService solrVendorCollectionService;

  @Mock
  private ProductReviewerRepository productReviewerRepository;

  @Captor
  private ArgumentCaptor<List<ProductDistributionTask>> productDistributionTaskArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Captor
  private ArgumentCaptor<Boolean> booleanArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<Product>> listProductArgumentCaptor;

  @InjectMocks
  private DistributionTaskServiceBean distributionTaskServiceBean;

  private GdnRestSingleResponse<ProductDetailResponse> gdnRestSingleResponse;

  private Product generateProduct() throws Exception {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setBusinessPartnerCode("EXTERNAL");
    product.setRestrictedKeywordsDetected(restrictedKeywordFieldJson);
    return product;
  }

  private Product generateProductInternal() throws Exception {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setBusinessPartnerCode("INTERNAL");
    return product;
  }

  private List<Product> generateProducts() throws Exception {
    List<Product> products = new ArrayList<Product>();
    products.add(this.generateProduct());
    return products;
  }

  private Vendor generateVendor() throws Exception {
    Vendor vendor = new Vendor();
    vendor.setVendorCode(VENDOR_ID);
    vendor.setSlaInDays(1);
    return vendor;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.openMocks(this);
    mapper = new ObjectMapper();
    Product product = this.generateProduct();
    List<Product> products = this.generateProducts();
    Vendor vendor = this.generateVendor();
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(this.productRepository.findByProductCode(Mockito.any()))
        .thenReturn(product);
    Mockito.when(
        this.autoDistributionConfigurationRepository.findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(
            Mockito.anyString(), Mockito.anyList())).thenReturn(VENDOR_ID);
    Mockito.when(this.vendorRepository.findByVendorCodeAndMarkForDeleteFalse(Mockito.anyString())).thenReturn(vendor);
    Mockito.when(this.productRepository.save(Mockito.any(Product.class))).thenReturn(null);
    Mockito.doNothing().when(this.distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.when(this.distributionTaskRepository.saveAll(Mockito.anyList())).thenReturn(null);
    Mockito.doNothing().when(this.distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.when(this.sequenceRepository.generateByCode(Mockito.anyString())).thenReturn(1L);
    Mockito.when(this.productRepository.findByProductCode(Mockito.any(), Mockito.any())).thenReturn(products);
    Mockito.when(this.productRepository.saveAll(Mockito.anyList())).thenReturn(null);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    vendorProduct = new Product();
    vendorProduct.setVendorId(VENDOR_ID);
    ReflectionTestUtils.setField(distributionTaskServiceBean, "autoDistributeSwitch", AUTO_DISTRIBUTE_SWITCH);
    ReflectionTestUtils.setField(distributionTaskServiceBean, "autoDistributeDefaultVendorCode", VENDOR_ID);

    imageQcProcessedResponse = new ImageQcProcessedResponse();
    imageQcProcessedResponse.setProductCode(PRODUCT_CODE);

    imageQcProcessedAndBrandResponse = new ImageQcProcessedAndBrandResponse();
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(imageQcProcessedResponse);

    RestrictedKeywordsByFieldResponse restrictedKeywordsByFieldResponse =
        new RestrictedKeywordsByFieldResponse(RestrictedKeywordFieldNames.PRODUCT_NAME.name(),
            List.of(RESTRICTED_KEYWORD));
    restrictedKeywordFieldJson = mapper.writeValueAsString(
        List.of(restrictedKeywordsByFieldResponse));


    Mockito.doNothing().when(this.solrVendorCollectionService)
        .assignProductToVendorAtomicUpdate(eq(STORE_ID), Mockito.anyString(), Mockito.anyList());

    ReflectionTestUtils.setField(distributionTaskServiceBean, "allowReplaceProductData",
        new StringBuilder(WorkflowState.REJECTED.name()).append(Constants.COMMA).append(WorkflowState.UNASSIGNED.name())
            .append(Constants.COMMA).append(WorkflowState.IN_REVIEW.name()).append(Constants.COMMA)
            .append(WorkflowState.EXCEEDED_SLA.name()).append(Constants.COMMA).append(WorkflowState.QC_REJECTED.name())
            .append(Constants.COMMA).append(WorkflowState.REJECTED.name()).toString());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.autoDistributionConfigurationRepository);
    Mockito.verifyNoMoreInteractions(this.vendorRepository);
    Mockito.verifyNoMoreInteractions(this.productRepository);
    Mockito.verifyNoMoreInteractions(this.productService);
    Mockito.verifyNoMoreInteractions(this.distributionTaskRepository);
    Mockito.verifyNoMoreInteractions(this.distributionTaskHistoryService);
    Mockito.verifyNoMoreInteractions(this.sequenceRepository);
    Mockito.verifyNoMoreInteractions(this.approvedProductPublisherService);
    Mockito.verifyNoMoreInteractions(this.productServiceRepository);
    Mockito.verifyNoMoreInteractions(this.solrVendorCollectionService);
    Mockito.verifyNoMoreInteractions(this.productReviewerRepository);
    Mockito.verifyNoMoreInteractions(this.pcbFeign);
  }

  @Test
   void autoDistributeTest() throws Exception {
    Mockito.when(this.productRepository.findByProductCode(Mockito.anyString())).thenReturn(null);
    this.distributionTaskServiceBean.autoDistribute(this.generateProduct(), true);
    Mockito.verify(this.productRepository).findByProductCode(Mockito.anyString());
    Mockito.verify(this.autoDistributionConfigurationRepository)
        .findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(this.vendorRepository).findByVendorCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productRepository).save(productArgumentCaptor.capture());
    Mockito.verify(this.distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(this.distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(this.distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(this.sequenceRepository).generateByCode(Mockito.anyString());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
    Mockito.verify(this.productReviewerRepository).save(Mockito.any(ProductReviewer.class));
  }

  @Test
   void autoDistributeSlaCalculateFalseTest() throws Exception {
    Mockito.when(this.productRepository.findByProductCode(Mockito.anyString())).thenReturn(null);
    this.distributionTaskServiceBean.autoDistribute(this.generateProduct(), true);
    Mockito.verify(this.productRepository).findByProductCode(Mockito.anyString());
    Mockito.verify(this.autoDistributionConfigurationRepository)
        .findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(this.vendorRepository).findByVendorCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productRepository).save(productArgumentCaptor.capture());
    Mockito.verify(this.distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(this.distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(this.distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(this.sequenceRepository).generateByCode(Mockito.anyString());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
    Mockito.verify(this.productReviewerRepository).save(Mockito.any(ProductReviewer.class));
  }

  @Test
   void notAutoDistributeForInternalAndNonReviewProductTest() throws Exception {
    Mockito.when(this.productRepository.findByProductCode(Mockito.anyString())).thenReturn(null);
    this.distributionTaskServiceBean.autoDistribute(this.generateProductInternal(), false);
    Mockito.verify(this.productRepository).findByProductCode(Mockito.anyString());
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
    Mockito.verify(this.productReviewerRepository).save(Mockito.any(ProductReviewer.class));
  }

  @Test
   void notAutoDistributeForNotForInternalAndReviewProductTest() throws Exception {
    Mockito.when(this.productRepository.findByProductCode(Mockito.anyString())).thenReturn(null);
    Product product = this.generateProductInternal();
    product.setReviewType(ReviewType.IMAGE);
    this.distributionTaskServiceBean.autoDistribute(product, true);
    Mockito.verify(this.productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(this.autoDistributionConfigurationRepository)
        .findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(eq(STORE_ID), Mockito.anyList());
    Mockito.verify(this.vendorRepository).findByVendorCodeAndMarkForDeleteFalse(eq(VENDOR_ID));
    Mockito.verify(this.productRepository).save(productArgumentCaptor.capture());
    Mockito.verify(this.distributionTaskRepository).updateProductDistributionTask(listProductArgumentCaptor.capture());
    Mockito.verify(this.distributionTaskRepository).saveAll(productDistributionTaskArgumentCaptor.capture());
    Mockito.verify(this.distributionTaskHistoryService).create(productDistributionTaskArgumentCaptor.capture());
    Mockito.verify(this.sequenceRepository).generateByCode(VENDOR_ID);
    Mockito.verify(this.productReviewerRepository).save(Mockito.any(ProductReviewer.class));
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
  }

  @Test
   void autoDistributeForPostLiveProduct() throws Exception {
    ReflectionTestUtils.setField(distributionTaskServiceBean, "autoDistributeSwitch", Boolean.FALSE);
    Product product = generateProduct();
    product.setPostLive(Boolean.TRUE);
    product.setReviewType(null);
    Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(null);
    this.distributionTaskServiceBean.autoDistribute(product, false);
    Mockito.verify(this.productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(this.vendorRepository).findByVendorCodeAndMarkForDeleteFalse(eq(VENDOR_ID));
    Mockito.verify(this.productRepository).save(productArgumentCaptor.capture());
    Mockito.verify(this.distributionTaskRepository).updateProductDistributionTask(listProductArgumentCaptor.capture());
    Mockito.verify(this.distributionTaskRepository).saveAll(productDistributionTaskArgumentCaptor.capture());
    Mockito.verify(this.distributionTaskHistoryService).create(productDistributionTaskArgumentCaptor.capture());
    Mockito.verify(this.sequenceRepository).generateByCode(VENDOR_ID);
    Assertions.assertTrue(productArgumentCaptor.getValue().isPostLive());
    Mockito.verify(this.productReviewerRepository).save(Mockito.any(ProductReviewer.class));
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
  }

  @Test
   void autoDistributeWithVendorIsNotFoundTest() throws Exception {
    Mockito.when(this.productRepository.findByProductCode(Mockito.anyString())).thenReturn(null);
    Mockito.when(this.vendorRepository.findByVendorCodeAndMarkForDeleteFalse(Mockito.anyString())).thenReturn(null);
    this.distributionTaskServiceBean.autoDistribute(this.generateProduct(), true);
    Mockito.verify(this.productRepository).findByProductCode(Mockito.anyString());
    Mockito.verify(this.autoDistributionConfigurationRepository)
        .findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(this.vendorRepository).findByVendorCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
    Mockito.verify(this.distributionTaskRepository, DistributionTaskServiceTest.NEVER_CALLED)
        .updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(this.distributionTaskRepository, DistributionTaskServiceTest.NEVER_CALLED).saveAll(Mockito.anyList());
    Mockito.verify(this.distributionTaskHistoryService, DistributionTaskServiceTest.NEVER_CALLED).create(
        Mockito.anyList());
    Mockito.verify(this.sequenceRepository, DistributionTaskServiceTest.NEVER_CALLED).generateByCode(
        Mockito.anyString());
    Mockito.verify(this.productReviewerRepository).save(Mockito.any(ProductReviewer.class));
  }

  @Test
   void autoDistributeWithOverwriteNotReplaceTest() throws Exception {
    this.distributionTaskServiceBean.autoDistribute(this.generateProduct(), true);
    Mockito.verify(this.productRepository).findByProductCode(Mockito.anyString());
  }

  @Test
   void autoDistributeWithOverwriteTest() throws Exception {
    Product product = this.generateProduct();
    product.setState(WorkflowState.IN_REVIEW);
    Mockito.when(this.productRepository.findByProductCode(Mockito.anyString()))
        .thenReturn(product);
    this.distributionTaskServiceBean.autoDistribute(product, true);
    Mockito.verify(this.productRepository)
        .findByProductCode(Mockito.anyString());
    Mockito.verify(this.productService).overwrite(Mockito.any(Product.class),
        productArgumentCaptor.capture());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
  }

  @Test
   void assigneeTest() throws Exception {
    Vendor vendor = this.generateVendor();
    vendor.setSlaInDays(null);
    Mockito.when(this.vendorRepository.findByVendorCodeAndMarkForDeleteFalse(Mockito.any()))
        .thenReturn(vendor);
    this.distributionTaskServiceBean.assignee(null, null);
    Mockito.verify(this.vendorRepository).findByVendorCodeAndMarkForDeleteFalse(Mockito.any());
    Mockito.verify(this.productRepository).findByProductCode(Mockito.any(), Mockito.anyList());
    Mockito.verify(this.distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(this.distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(this.productRepository).saveAll(Mockito.anyList());
    Mockito.verify(this.distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(this.sequenceRepository).generateByCode(Mockito.anyString());
    Mockito.verify(this.solrVendorCollectionService)
        .assignProductToVendorAtomicUpdate(eq(STORE_ID), Mockito.any(), Mockito.any());
  }

  @Test
   void publishVendorApprovedEventTest() throws Exception {
    distributionTaskServiceBean.publishVendorApprovedEvent(generateProduct(), false);
    Mockito.verify(approvedProductPublisherService).publishVendorApprovedEvent(generateProduct(), false);
  }

  @Test
   void assignee_2_Test() throws Exception {
    Vendor vendor = this.generateVendor();
    vendor.setSlaInDays(0);
    Mockito.when(this.vendorRepository.findByVendorCodeAndMarkForDeleteFalse(Mockito.any())).thenReturn(vendor);
    this.distributionTaskServiceBean.assignee(null, null);
    Mockito.verify(this.vendorRepository).findByVendorCodeAndMarkForDeleteFalse(Mockito.any());
    Mockito.verify(this.productRepository).findByProductCode(Mockito.any(), Mockito.anyList());
    Mockito.verify(this.distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(this.distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(this.productRepository).saveAll(Mockito.anyList());
    Mockito.verify(this.distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(this.sequenceRepository).generateByCode(Mockito.anyString());
    Mockito.verify(this.solrVendorCollectionService)
        .assignProductToVendorAtomicUpdate(eq(STORE_ID), Mockito.any(), Mockito.any());
  }

  @Test
   void assigneeWithInvalidVendorCodeExceptionTest() throws Exception {
    Mockito.when(this.vendorRepository.findByVendorCodeAndMarkForDeleteFalse(Mockito.any()))
      .thenReturn(null);
    Assertions.assertThrows(Exception.class,
      () -> this.distributionTaskServiceBean.assignee(null, null));
    Mockito.verify(this.vendorRepository).findByVendorCodeAndMarkForDeleteFalse(Mockito.any());
    Mockito.verify(this.productRepository, DistributionTaskServiceTest.NEVER_CALLED)
      .findByProductCode(Mockito.anyList(), Mockito.anyList());
    Mockito.verify(this.distributionTaskRepository, DistributionTaskServiceTest.NEVER_CALLED)
      .updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(this.distributionTaskRepository, DistributionTaskServiceTest.NEVER_CALLED)
      .saveAll(Mockito.anyList());
    Mockito.verify(this.productRepository, DistributionTaskServiceTest.NEVER_CALLED)
      .saveAll(Mockito.anyList());
    Mockito.verify(this.distributionTaskHistoryService, DistributionTaskServiceTest.NEVER_CALLED)
      .create(Mockito.anyList());
    Mockito.verify(this.sequenceRepository, DistributionTaskServiceTest.NEVER_CALLED)
      .generateByCode(Mockito.anyString());
  }

  @Test
   void clearPresentDistributionTaskAndCreateNewTaskTest() throws Exception {
    Mockito.when(vendorRepository.findById(VENDOR_ID)).thenReturn(Optional.of(this.generateVendor()));
    distributionTaskServiceBean.clearPresentDistributionTaskAndCreateNewTask(STORE_ID, vendorProduct);
    Mockito.verify(this.vendorRepository).findById(VENDOR_ID);
    Mockito.verify(this.distributionTaskRepository)
        .updateProductDistributionTask(Collections.singletonList(vendorProduct));
    Mockito.verify(this.distributionTaskRepository).saveAll(productDistributionTaskArgumentCaptor.capture());
    Mockito.verify(this.sequenceRepository).generateByCode(Mockito.anyString());
    Assertions.assertEquals(STATE_IN_REVIW,
        productDistributionTaskArgumentCaptor.getValue().get(0).getState().name());
  }

  @Test
   void autoDistributeTest_SwitchedOff() throws Exception {
    ReflectionTestUtils.setField(distributionTaskServiceBean, "autoDistributeSwitch", Boolean.FALSE);
    Mockito.when(this.productRepository.findByProductCode(Mockito.anyString())).thenReturn(null);
    this.distributionTaskServiceBean.autoDistribute(this.generateProduct(), true);
    Mockito.verify(this.productRepository).findByProductCode(Mockito.anyString());
    Mockito.verify(this.vendorRepository).findByVendorCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
    Mockito.verify(this.distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(this.distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(this.distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(this.sequenceRepository).generateByCode(Mockito.anyString());
    Mockito.verify(this.productReviewerRepository).save(Mockito.any(ProductReviewer.class));
  }

  @Test
   void getProductDetailByProductCode() throws Exception {
    gdnRestSingleResponse =
        new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, new ProductDetailResponse(),
            REQUEST_ID);
    Mockito.when(pcbFeign.getProductDetailByProductCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), eq(PRODUCT_CODE), eq(true)))
        .thenReturn(gdnRestSingleResponse);
    this.distributionTaskServiceBean.getProductDetailByProductCode(USERNAME, PRODUCT_CODE);
    Mockito.verify(pcbFeign).getProductDetailByProductCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), eq(PRODUCT_CODE), eq(true));
  }

  @Test
   void getProductDetailByProductCodeSuccessFalse() throws Exception {
    gdnRestSingleResponse =
        new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, new ProductDetailResponse(),
            REQUEST_ID);
    Mockito.when(pcbFeign.getProductDetailByProductCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), eq(PRODUCT_CODE), eq(true)))
        .thenReturn(gdnRestSingleResponse);
    try {
      this.distributionTaskServiceBean.getProductDetailByProductCode(USERNAME, PRODUCT_CODE);
    } catch (ApplicationException e) {
    } finally {
      Mockito.verify(pcbFeign).getProductDetailByProductCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), eq(PRODUCT_CODE), eq(true));
    }
  }

  @Test
   void getImageQcResponseByProductCodeTest() {
    Mockito.when(productServiceRepository.getImageQcPredictionResponse(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    ImageQcProcessedAndBrandResponse response = distributionTaskServiceBean.getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(productServiceRepository).getImageQcPredictionResponse(PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE, response.getImageQcProcessedResponse().getProductCode());
  }
}
