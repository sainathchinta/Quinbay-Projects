package com.gdn.x.mta.distributiontask.controller;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.service.api.QCTaskWrapperService;

import org.apache.commons.lang3.StringUtils;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.ProductApprovalDetailStatusEvent;
import com.gdn.x.mta.distributiontask.controller.util.ProductConverterUtil;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.RejectProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.constant.QCFilterState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.QCTaskService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ProductApprovalStatusPublisherService;

/**
 * Created by Poornima on 9/22/16.
 */
public class QCControllerTest {

    private static final String STORE_ID = "store-id";
    private static final String CHANNEL_ID = "channel-id";
    private static final String CLIENT_ID = "client-id";
    private static final String USER_NAME = "user-name";
    private static final String REQUEST_ID = "request-id";
    private static final String TASK_CODE = "task_code-id";
    private static final String PRODUCT_CODE = "product-code";
    private static final String PRODUCT_ID = "123";
    private static final String DATE = "2016-12-12 00:00";
    private static final String STATUS_ALL = QCFilterState.ALL.toString();


  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private Product product;

    @InjectMocks private QCController qcController;

    @Mock private ProductService productService;

    @Mock
    private QCTaskService qcTaskService;

    @Mock
    private QCTaskWrapperService qcTaskWrapperService;

    @Mock
    private ProductConverterUtil productConverterUtil;

    @Mock
    private ProductDistributionTaskService productDistributionTaskService;

    @Mock
    private ApprovedProductPublisherService approvedProductPublisherService;

    @Mock
    private ProductApprovalStatusPublisherService productApprovalStatusPublisherService;

    private RejectProductRequest rejectProductRequest;

    private ProductDistributionTask productDistributionTask;

    private MockMvc mockMvc;

    private ProductApprovalDetailStatusEvent productApprovalDetailStatusEvent;

  private ObjectMapper objectMapper = new ObjectMapper();

    private List<Product> createProductList() {
        Product product = new Product.Builder().productName("productName").productCode("code")
            .productAttributes(new ArrayList<ProductAttribute>())
            .productImages(new ArrayList<ProductImage>()).productItems(new ArrayList<ProductItem>())
            .build();
        Vendor currentVendor = new Vendor();
        currentVendor.setName("Vendor1");
        currentVendor.setStoreId("10001");
        currentVendor.setUpdatedDate(new Date());
        currentVendor.setMarkForDelete(false);
        currentVendor.setAbleToReject(false);
        currentVendor.setQcRequired(false);
        currentVendor.setSlaInDays(10);
        currentVendor.setId("31hj134-jbeeqw8-jkdsa2-ksdq234");
        product.setCurrentVendor(currentVendor);
        List<Product> productList = new ArrayList<>();
        productList.add(product);
        return productList;
    }

    @BeforeEach public void setUp() {
      MockitoAnnotations.openMocks(this);
      this.mockMvc = MockMvcBuilders.standaloneSetup(this.qcController).build();
      this.rejectProductRequest = new RejectProductRequest(PRODUCT_ID, "QC_REJECTED", "reason", true);
      this.product = new Product.Builder().productName("productName").productCode("code").productAttributes(new ArrayList<ProductAttribute>())
        .productImages(new ArrayList<ProductImage>()).productItems(new ArrayList<ProductItem>()).build();
      productDistributionTask =
        new ProductDistributionTask(TASK_CODE, new Vendor(), this.product, WorkflowState.IN_REVIEW,
          new Date());
    }

  @Test
   void rejectProductTest() throws Exception {
    String request = OBJECT_MAPPER.writeValueAsString(this.rejectProductRequest);
    Mockito.when(this.productService.findByProductId(PRODUCT_ID)).thenReturn(this.product);
    this.mockMvc
        .perform(MockMvcRequestBuilders.post(QCController.BASE_PATH + QCController.REJECT_PRODUCT)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USER_NAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.productService, Mockito.times(1)).findByProductId(PRODUCT_ID);
    Mockito.verify(this.qcTaskWrapperService, Mockito.times(1)).qcRejectProduct(product,
        rejectProductRequest.isAssignedToVendor(), rejectProductRequest.getRejectedReason(),
        WorkflowState.QC_REJECTED);
  }

  @Test
   void rejectProductExceptionTest() throws Exception {
    String request = OBJECT_MAPPER.writeValueAsString(this.rejectProductRequest);
    Mockito.when(this.productService.findByProductId(PRODUCT_ID)).thenThrow(Exception.class);
    this.mockMvc
        .perform(MockMvcRequestBuilders.post(QCController.BASE_PATH + QCController.REJECT_PRODUCT)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USER_NAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.productService, Mockito.times(1)).findByProductId(PRODUCT_ID);
    Mockito.verify(this.qcTaskWrapperService, Mockito.times(0)).qcRejectProduct(product,
        rejectProductRequest.isAssignedToVendor(), rejectProductRequest.getRejectedReason(),
        WorkflowState.QC_REJECTED);
  }

  @Test
   void approveProductTest() throws Exception {
    Mockito.when(this.productService.findByProductId(PRODUCT_ID)).thenReturn(this.product);
    Mockito.when(this.productDistributionTaskService.findByProductId(PRODUCT_ID)).thenReturn(this.productDistributionTask);
    this.mockMvc
        .perform(MockMvcRequestBuilders.post(QCController.BASE_PATH + QCController.APPROVE_PRODUCT)
            .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USER_NAME)
            .param("productId", PRODUCT_ID))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1)).findByProductId(PRODUCT_ID);
    Mockito.verify(this.qcTaskService, Mockito.times(1)).approveProductByQC(productDistributionTask);
  }

  @Test
   void approveProductProductIdIsEmptyExceptionTest() throws Exception {
    Mockito.when(this.productService.findByProductId(PRODUCT_ID)).thenReturn(this.product);
    Mockito.when(this.productDistributionTaskService.findByProductId(PRODUCT_ID)).thenReturn(this.productDistributionTask);
    this.mockMvc
        .perform(MockMvcRequestBuilders.post(QCController.BASE_PATH + QCController.APPROVE_PRODUCT)
            .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USER_NAME)
            .param("productId", ""))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.productDistributionTaskService, Mockito.times(0)).findByProductId(PRODUCT_ID);
    Mockito.verify(this.qcTaskService, Mockito.times(0)).approveProductByQC(productDistributionTask);
  }

  @Test
   void approveProductProductDistributionTaskNullExceptionTest() throws Exception {
    Mockito.when(this.productService.findByProductId(PRODUCT_ID)).thenReturn(this.product);
    Mockito.when(this.productDistributionTaskService.findByProductId(PRODUCT_ID)).thenReturn(null);
    this.mockMvc
        .perform(MockMvcRequestBuilders.post(QCController.BASE_PATH + QCController.APPROVE_PRODUCT)
            .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USER_NAME)
            .param("productId", PRODUCT_ID))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1)).findByProductId(PRODUCT_ID);
    Mockito.verify(this.qcTaskService, Mockito.times(0)).approveProductByQC(productDistributionTask);
  }

  @Test
   void moveFailedProductToQCTest() throws Exception {
    Mockito.when(
        this.productDistributionTaskService
            .findTopByProductProductCodeOrderByUpdatedDateDesc(PRODUCT_CODE)).thenReturn(
        this.productDistributionTask);
    this.mockMvc
        .perform(
            MockMvcRequestBuilders
                .post(QCController.BASE_PATH + QCController.MOVE_FAILED_PRODUCT_TO_QC)
                .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USER_NAME)
                .param("productCode", PRODUCT_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1))
        .findTopByProductProductCodeOrderByUpdatedDateDesc(PRODUCT_CODE);
    Mockito.verify(this.qcTaskService, Mockito.times(1)).moveFailedProductToQC(
        productDistributionTask);
  }

  @Test
   void moveFailedProductToQC_product_code_empty_exception_Test() throws Exception {
    this.mockMvc
        .perform(
            MockMvcRequestBuilders
                .post(QCController.BASE_PATH + QCController.MOVE_FAILED_PRODUCT_TO_QC)
                .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USER_NAME)
                .param("productCode", "")).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.productDistributionTaskService, Mockito.times(0))
        .findTopByProductProductCodeOrderByUpdatedDateDesc(PRODUCT_CODE);
    Mockito.verify(this.qcTaskService, Mockito.times(0)).moveFailedProductToQC(
        productDistributionTask);
  }

  @Test
   void moveFailedProductToQC_product_distribution_task_empty_exception_Test()
      throws Exception {
    Mockito.when(
        this.productDistributionTaskService
            .findTopByProductProductCodeOrderByUpdatedDateDesc(PRODUCT_CODE)).thenReturn(null);
    this.mockMvc
        .perform(
            MockMvcRequestBuilders
                .post(QCController.BASE_PATH + QCController.MOVE_FAILED_PRODUCT_TO_QC)
                .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USER_NAME)
                .param("productCode", PRODUCT_CODE)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1))
        .findTopByProductProductCodeOrderByUpdatedDateDesc(PRODUCT_CODE);
    Mockito.verify(this.qcTaskService, Mockito.times(0)).moveFailedProductToQC(
        productDistributionTask);
  }

  @Test
   void retryQcProductsTest() throws Exception {
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(QCController.BASE_PATH + QCController.RETRY_QC_PRODUCTS).accept(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USER_NAME).param("qcRetryCount", String.valueOf(1))
        .param("deltaHours", String.valueOf(2)).param("batchSize", String.valueOf(3)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(qcTaskService).retryQcProducts(1, 2, 3);
  }

  @Test
   void filterProductSummaryTest2() throws Exception {
    List<Product> productList = createProductList();
    ProductListRequest productListRequest = new ProductListRequest();
    productListRequest.setStartDate(DATE);
    productListRequest.setEndDate(DATE);
    Page<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOPage = new PageImpl<>(
        List.of(new ProductAndReviewerDetailsDTO(productList.get(0), new ProductReviewer())));
    when(qcTaskService.getFilterProductSummaryFromSolr(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(ProductListRequest.class), Mockito.any(Pageable.class))).thenReturn(
      productAndReviewerDetailsDTOPage);
    when(productConverterUtil.convertToDistributionProductResponse(productAndReviewerDetailsDTOPage, StringUtils.EMPTY))
        .thenReturn(new ArrayList<>());
    qcController.filterProductSummary(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USER_NAME, STATUS_ALL, 0, 25,
        productListRequest);
    verify(qcTaskService).getFilterProductSummaryFromSolr(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(ProductListRequest.class), Mockito.any(Pageable.class));
    Mockito.verify(productConverterUtil)
        .convertToDistributionProductResponse(productAndReviewerDetailsDTOPage, null);
  }

  @AfterEach
  public void shutDown(){
      Mockito.verifyNoMoreInteractions(productApprovalStatusPublisherService);
      Mockito.verifyNoMoreInteractions(qcTaskWrapperService);
  }
}
