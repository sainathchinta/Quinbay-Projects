package com.gdn.x.mta.distributiontask.controller;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.request.VendorDefaultFilterRequest;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
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

import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.VendorCapacityDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorTaskInformationDTO;
import com.gdn.x.mta.distributiontask.request.VendorDetailRequest;
import com.gdn.x.mta.distributiontask.service.api.ProductImageQcFeedbackService;
import com.gdn.x.mta.distributiontask.service.api.VendorService;

/**
 * Created by Alok on 9/19/16.
 */
public class VendorControllerTest {

  private static final String STORE_ID = "store-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String CLIENT_ID = "client-id";
  private static final String USER_NAME = "user-name";
  private static final String REQUEST_ID = "request-id";
  private static final String VENDOR_CODE ="vendorCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String VENDOR_EMAIL = "vendorEmail";
  private static final ObjectMapper objectMapper = new ObjectMapper();

  @InjectMocks
  private VendorController vendorController;

  @Mock
  private VendorService vendorService;

  @Mock
  private ProductDistributionTaskService productDistributionTaskService;

  @Mock
  private ProductImageQcFeedbackService productImageQcFeedbackService;

  private MockMvc mockMvc;

  private VendorDetailRequest createVendorDetailRequest() {
    VendorDetailRequest vendorDetailRequest = new VendorDetailRequest();
    vendorDetailRequest.setName("Name");
    vendorDetailRequest.setQuota(2);
    vendorDetailRequest.setVendorCode("VendorCode");
    return vendorDetailRequest;
  }

  private List<Vendor> createVendorList() {
    List<Vendor> vendorList = new ArrayList<>();
    vendorList.add(
        new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").isAbleToReject(false)
            .isQcRequired(false).build());
    return vendorList;
  }

  private List<VendorCapacityDTO> createVendorCapacity() {
    return new ArrayList<>(
        Arrays.asList(new VendorCapacityDTO("1", "vendorCode", "name", new Date(), new Date(), 1)));
  }

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.vendorController).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productImageQcFeedbackService);
  }

  @Test public void saveTestOk() throws Exception {
    Vendor vendor = new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").isAbleToReject(false)
        .isQcRequired(false).build();
    Mockito.when(vendorService.save(Mockito.any(Vendor.class))).thenReturn(vendor);
    vendorController
        .save(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USER_NAME, createVendorDetailRequest());
    Mockito.verify(vendorService).save(Mockito.any(Vendor.class));
  }

  @Test public void saveTestNotOk() throws Exception {
    vendorController
        .save(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USER_NAME, createVendorDetailRequest());
    Mockito.verify(vendorService).save(Mockito.any(Vendor.class));
  }

  @Test public void getVendorListTest() throws Exception {
    Page<Vendor> vendorList = new PageImpl<Vendor>(createVendorList());
    Mockito.when(vendorService.findVendorList(Mockito.any(Pageable.class))).thenReturn(vendorList);
    vendorController.getVendorList(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USER_NAME, 0, 25);
  }

  @Test public void getVendorListTestNotOk() throws Exception {
    Page<Vendor> vendorList = new PageImpl<Vendor>(createVendorList());
    vendorController.getVendorList(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USER_NAME, 0, 25);
  }

  @Test public void getVendorByCodeTestOk() throws Exception {
    Vendor vendor =
        new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").isQcRequired(false)
            .isAbleToReject(false).build();
    Mockito.when(vendorService.findByVendorCode(Mockito.anyString())).thenReturn(vendor);
    vendorController
        .getVendorByCode(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USER_NAME, "vendor-Code");
  }

  @Test public void getVendorByCodeTestNotOk() throws Exception {
    Vendor vendor =
        new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").isQcRequired(false)
            .isAbleToReject(false).build();
    vendorController
        .getVendorByCode(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USER_NAME, "vendor-Code");
  }

  @Test
   void countVendorsCapacityTest() throws Exception{
    Mockito.when(this.vendorService
        .countVendorsCapacity()).thenReturn(createVendorCapacity());
    this.mockMvc.perform(MockMvcRequestBuilders.get(VendorController.BASE_PATH+VendorController.GET_VENDORS_CAPACITY)
        .accept(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USER_NAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.vendorService, Mockito.times(1)).countVendorsCapacity();
  }

  @Test
   void countVendorsCapacityExceptionTest() throws Exception{
    Mockito.when(this.vendorService
        .countVendorsCapacity()).thenThrow(RuntimeException.class);
    this.mockMvc.perform(MockMvcRequestBuilders.get(VendorController.BASE_PATH+VendorController.GET_VENDORS_CAPACITY)
        .accept(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USER_NAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.vendorService, Mockito.times(1)).countVendorsCapacity();
  }

  @Test
   void countVendorsInformationTaskExceptionTest() throws Exception{
    Mockito.when(this.vendorService
        .countVendorAssignationAndCapacity()).thenThrow(RuntimeException.class);
    this.mockMvc.perform(MockMvcRequestBuilders.get(VendorController.BASE_PATH+VendorController.GET_VENDORS_INFORMATION_TASK)
        .accept(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USER_NAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.vendorService, Mockito.times(1)).countVendorAssignationAndCapacity();
  }

  @Test
   void countVendorsInformationTaskTest() throws Exception{
    Mockito.when(this.vendorService
        .countVendorAssignationAndCapacity()).thenReturn(new ArrayList<VendorTaskInformationDTO>());
    this.mockMvc.perform(MockMvcRequestBuilders.get(VendorController.BASE_PATH+VendorController.GET_VENDORS_INFORMATION_TASK)
        .accept(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USER_NAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.vendorService, Mockito.times(1)).countVendorAssignationAndCapacity();
  }

  @Test public void countProductAssignedToVendorTestOk() throws Exception {
    Vendor vendor =
        new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").isQcRequired(false)
            .isAbleToReject(false).build();
    Integer count = 0;
    Mockito.when(this.vendorService.assignedProductCount(Mockito.anyString()))
        .thenReturn(count);
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(VendorController.BASE_PATH + VendorController.GET_CURRENTLY_ASSIGNED_PRODUCT)
        .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USER_NAME).param("vendorCode", VENDOR_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.vendorService, Mockito.times(1))
        .assignedProductCount(Mockito.anyString());
    int i = 0;
    Integer intObj = new Integer(i);
    Assertions.assertEquals(intObj, count);
  }

  @Test public void countProductAssignedToVendorTestNotOk() throws Exception {
    Vendor vendor =
        new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").isQcRequired(false)
            .isAbleToReject(false).build();
    Mockito.when(this.vendorService.assignedProductCount(Mockito.anyString()))
        .thenThrow(Exception.class);
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(VendorController.BASE_PATH + VendorController.GET_CURRENTLY_ASSIGNED_PRODUCT)
        .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USER_NAME).param("vendorCode", VENDOR_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.vendorService)
        .assignedProductCount(Mockito.anyString());

  }

  @Test public void countProductAsssignedToVendorWhereVendorCodeIsInvalid() throws Exception {
    Mockito.when(this.vendorService.assignedProductCount(Mockito.anyString()))
        .thenThrow(Exception.class);
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(VendorController.BASE_PATH + VendorController.GET_CURRENTLY_ASSIGNED_PRODUCT)
        .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USER_NAME).param("vendorCode", VENDOR_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.vendorService, Mockito.times(1))
        .assignedProductCount(Mockito.anyString());
  }

  @Test public void deleteVendorTestOk() throws Exception {
    Vendor vendor =
        new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").isQcRequired(false)
            .isAbleToReject(false).build();
    Mockito.when(this.vendorService.findByVendorCode(Mockito.anyString()))
        .thenReturn(vendor);
    Mockito.doNothing().when(this.productDistributionTaskService)
        .movingProductsbackToProductDistribution(Mockito.any(Vendor.class));
    Mockito.doNothing().when(this.vendorService)
        .deleteVender(Mockito.any(Vendor.class));
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(VendorController.BASE_PATH + VendorController.DELETE_VENDOR)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USER_NAME).param("vendorCode", VENDOR_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true
        )));
    Mockito.verify(this.vendorService)
        .findByVendorCode(Mockito.anyString());
    Mockito.verify(this.productDistributionTaskService)
        .movingProductsbackToProductDistribution(Mockito.any(Vendor.class));
  }

  @Test
   void deleteVendorTestNotOk() throws Exception {
    Vendor vendor =
        new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").isQcRequired(false)
            .isAbleToReject(false).build();
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(VendorController.BASE_PATH + VendorController.DELETE_VENDOR)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USER_NAME).param("vendorCode", VENDOR_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.vendorService)
        .findByVendorCode(Mockito.anyString());
  }

  @Test
   void deleteVendorWhereVendorCodeIsInvalid() throws Exception {
   Vendor vendor = new Vendor.Builder().vendorCode(null).id("ID").name("name").isQcRequired(false)
        .isAbleToReject(false).build();
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(VendorController.BASE_PATH + VendorController.DELETE_VENDOR)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USER_NAME).param("vendorCode", VENDOR_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.vendorService)
        .findByVendorCode(Mockito.anyString());
  }

  @Test
   void getImageFeedbackDetailTest() throws Exception {
    this.mockMvc.perform(MockMvcRequestBuilders.get(
        VendorController.BASE_PATH + VendorController.GET_IMAGE_FEEDBACK_DETAIL.replace("{productCode}", "")
            + PRODUCT_CODE).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USER_NAME)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(productImageQcFeedbackService).findProductQcFeedbackResponseByProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
   void getImageFeedbackDetailExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(productImageQcFeedbackService)
        .findProductQcFeedbackResponseByProductCode(STORE_ID, PRODUCT_CODE);
    this.mockMvc.perform(MockMvcRequestBuilders.get(
        VendorController.BASE_PATH + VendorController.GET_IMAGE_FEEDBACK_DETAIL.replace("{productCode}", "")
            + PRODUCT_CODE).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USER_NAME)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(productImageQcFeedbackService).findProductQcFeedbackResponseByProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
   void saveDefaultSettingTest() throws Exception {
    VendorDefaultFilterRequest vendorDefaultFilterRequest = new VendorDefaultFilterRequest();
    mockMvc.perform(MockMvcRequestBuilders
            .post(VendorController.BASE_PATH
                    + VendorController.SAVE_DEFAULT_SETTING)
            .contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(vendorDefaultFilterRequest))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USER_NAME)).andExpect(status().isOk());
    Mockito.verify(this.vendorService).saveDefaultSettingFilter(Mockito.any(), Mockito.anyString());
  }
  @Test
   void saveDefaultSettingExceptionTest() throws Exception {
    VendorDefaultFilterRequest vendorDefaultFilterRequest = new VendorDefaultFilterRequest();
    Mockito.doThrow(RuntimeException.class).when(vendorService)
            .saveDefaultSettingFilter(vendorDefaultFilterRequest, STORE_ID);
    mockMvc.perform(MockMvcRequestBuilders
            .post(VendorController.BASE_PATH
                    + VendorController.SAVE_DEFAULT_SETTING)
            .contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(vendorDefaultFilterRequest))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USER_NAME)).andExpect(status().isOk());
    Mockito.verify(this.vendorService).saveDefaultSettingFilter(Mockito.any(), Mockito.anyString());
  }
}
