package com.gdn.partners.pbp.controller.productlevel3;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.gda.mta.product.dto.response.AvailableToCopyItemDetailsResponse;
import com.gda.mta.product.dto.response.AvailableToCopyProductDetailsResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductLevel3SummaryMinifiedRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryMinifiedResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.mta.product.converter.ProductLevel3RequestConverter;
import com.gdn.mta.product.converter.ProductLevel3ResponseConverter;
import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3ImageBundle;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3GdnSkuListRequest;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemSearchRequest;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemSearchResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3UpdateItemSyncStockRequest;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3UpdateSyncStockBusinessPartnerRequest;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Item;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3ItemSearch;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Service;
import com.gdn.x.base.controller.GlobalControllerAdvice;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryCountWebItemSkuOOSResponseDTO;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;

import static org.mockito.Mockito.eq;

public class ProductLevel3ControllerTest {

  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";

  private static final String BUSINESS_PARTNER_CODE = "BusinessPartnerCode";
  private static final String LINKED_BUSINESS_PARTNER_CODE = "LinkedBusinessPartnerCode";
  private static final String GDN_SKU = "gdnSKU";
  private static final boolean SYNC_STOCK = true;
  private static final long COUNT = 1;
  private static final String ASSIGNED_BY = "assignedBy";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String PRODUCT_CODE = "productCode";
  private static final String ITEM_CODE = "itemCode";

  private static final Pageable DEFAULT_PAGEABLE = PageRequest.of(0, 10);

  @InjectMocks
  private ProductLevel3Controller productLevel3Controller;
  @Mock
  private ProductLevel3Service productLevel3Service;
  @Mock
  private ProductLevel3RequestConverter requestConverter;
  @Mock
  private ProductLevel3ResponseConverter responseConverter;

  @Mock
  private ProductService productService;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Captor
  private ArgumentCaptor<ProductLevel3SummaryRequest> productLevel3SummaryRequestCaptor;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  private WebInventoryCountWebItemSkuOOSResponseDTO webInventoryCountWebItemSkuOOSResponseDTO;
  private ProductLevel3UpdateSyncStockBusinessPartnerRequest productLevel3UpdateSyncStockBusinessPartnerRequest;
  private ProductLevel3ImageBundle productLevel3ImageBundle;


  private Page<ProductLevel3Summary> generateProductLevel3Summary() {
    List<ProductLevel3Summary> productLevel3Summary = new ArrayList<ProductLevel3Summary>();
    ProductLevel3Summary productLevel3SummaryItem = new ProductLevel3Summary();
    productLevel3SummaryItem.setPrices(new ArrayList<ProductLevel3Price>());
    productLevel3SummaryItem.setViewConfigs(new ArrayList<ProductLevel3ViewConfig>());
    productLevel3SummaryItem.setImages(new ArrayList<ProductLevel3Image>());
    productLevel3SummaryItem.getPrices().add(new ProductLevel3Price());
    productLevel3SummaryItem.getViewConfigs().add(new ProductLevel3ViewConfig());
    productLevel3SummaryItem.getImages().add(new ProductLevel3Image());
    productLevel3SummaryItem.setNonDistributionReserved(10);
    productLevel3SummaryItem.setNonDistributionAvailable(20);
    productLevel3SummaryItem.setProductScore(90.0);
    productLevel3Summary.add(productLevel3SummaryItem);
    return new PageImpl<ProductLevel3Summary>(productLevel3Summary, DEFAULT_PAGEABLE, 1);
  }

  private Page<ProductLevel3SummaryMinified> generateProductLevel3SummaryMinified() {
    List<ProductLevel3SummaryMinified> productDatasList =
        new ArrayList<ProductLevel3SummaryMinified>();
    ProductLevel3SummaryMinified productData = new ProductLevel3SummaryMinified();
    productData.setImages(new ArrayList<ProductLevel3Image>());
    productData.setPrices(new ArrayList<ProductLevel3Price>());
    productData.setViewConfigs(new ArrayList<ProductLevel3ViewConfig>());
    productData.getImages().add(new ProductLevel3Image(true, 1, "localtionPath"));
    productData.getPrices().add(new ProductLevel3Price(CHANNEL_ID, 1000.0, 1000.0));
    productData.getViewConfigs().add(new ProductLevel3ViewConfig(CHANNEL_ID, true, true));
    productDatasList.add(productData);
    return new PageImpl<ProductLevel3SummaryMinified>(productDatasList, DEFAULT_PAGEABLE, 10);
  }

  private Page<ProductLevel3ImageBundle> generateProductLevel3ImageBundle() {
    ProductLevel3ImageBundle imageBundle =
        new ProductLevel3ImageBundle(GDN_SKU, Arrays.asList(new ProductLevel3Image()));
    List<ProductLevel3ImageBundle> imageBundles = Arrays.asList(imageBundle);
    return new PageImpl<>(imageBundles);
  }

  @BeforeEach
  public void __initialize() throws Exception {
    initMocks(this);

    this.mockMvc =
        MockMvcBuilders
            .standaloneSetup(this.productLevel3Controller)
            .setMessageConverters(new ByteArrayHttpMessageConverter(),
                new StringHttpMessageConverter(), new ResourceHttpMessageConverter(),
                new FormHttpMessageConverter(), new MappingJackson2HttpMessageConverter())
                .setControllerAdvice(new GlobalControllerAdvice()).build();
    this.objectMapper = new ObjectMapper();

    this.webInventoryCountWebItemSkuOOSResponseDTO =
        new WebInventoryCountWebItemSkuOOSResponseDTO(ProductLevel3ControllerTest.COUNT);
    this.productLevel3UpdateSyncStockBusinessPartnerRequest =
        new ProductLevel3UpdateSyncStockBusinessPartnerRequest();
    this.productLevel3UpdateSyncStockBusinessPartnerRequest
        .setBusinessPartnerCode(ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE);
    this.productLevel3UpdateSyncStockBusinessPartnerRequest
        .setSyncStock(ProductLevel3ControllerTest.SYNC_STOCK);

    when(this.productLevel3Service.findImageBundleByFilter(Mockito.any(ItemSummaryRequest.class),
        Mockito.any(Pageable.class), Mockito.any(SortOrder.class)))
            .thenReturn(this.generateProductLevel3ImageBundle());
  }

  @AfterEach
  public void _finalize() {
    verifyNoMoreInteractions(this.productLevel3Service);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.productServiceWrapper);
  }

  @Test
  public void countWebItemSkuOOSTest_checkArg_businessPartnerCode() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.COUNT_ITEM_OOS)
            .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID)
            .addParameter("businessPartnerCode", "").build();
    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void updateSyncStockBusinessPartner() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_SYNC_STOCK_BUSINESS_PARTNER)
            .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID).build();
    this.mockMvc
        .perform(
            post(uri)
                .accept(MediaType.APPLICATION_JSON_VALUE)
                .contentType(MediaType.APPLICATION_JSON)
                .content(
                    this.objectMapper
                        .writeValueAsString(this.productLevel3UpdateSyncStockBusinessPartnerRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.productLevel3Service).updateSyncStockByBusinessPartnerCode(
        ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE, ProductLevel3ControllerTest.SYNC_STOCK);
  }

  @Test
  public void updateSyncStockBusinessPartner_checkArg_businessPartnerCode() throws Exception {
    this.productLevel3UpdateSyncStockBusinessPartnerRequest.setBusinessPartnerCode("");
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_SYNC_STOCK_BUSINESS_PARTNER)
            .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID).build();

    try {
      this.mockMvc.perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON).content(this.objectMapper.writeValueAsString(this.productLevel3UpdateSyncStockBusinessPartnerRequest)))
          .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void updateSyncStockBusinessPartner_checkArg_syncStock() throws Exception {
    this.productLevel3UpdateSyncStockBusinessPartnerRequest.setSyncStock(null);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_SYNC_STOCK_BUSINESS_PARTNER)
            .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID).build();

    try {
      this.mockMvc.perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
              .content(this.objectMapper.writeValueAsString(this.productLevel3UpdateSyncStockBusinessPartnerRequest)))
          .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ProductLevel3ControllerErrorMessage.SYNC_STOCK_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void updateSyncStockBusinessPartner_failed() throws Exception {
    doThrow(new RuntimeException()).when(this.productLevel3Service)
        .updateSyncStockByBusinessPartnerCode(ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE,
            ProductLevel3ControllerTest.SYNC_STOCK);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_SYNC_STOCK_BUSINESS_PARTNER)
            .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID).build();
    this.mockMvc
        .perform(
            post(uri)
                .accept(MediaType.APPLICATION_JSON_VALUE)
                .contentType(MediaType.APPLICATION_JSON)
                .content(
                    this.objectMapper
                        .writeValueAsString(this.productLevel3UpdateSyncStockBusinessPartnerRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.productLevel3Service).updateSyncStockByBusinessPartnerCode(
        ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE, ProductLevel3ControllerTest.SYNC_STOCK);
  }


  @Test
  public void updateItemSyncStock() throws Exception {
    ProductLevel3UpdateItemSyncStockRequest productLevel3UpdateItemSyncStockRequest =
        new ProductLevel3UpdateItemSyncStockRequest();
    productLevel3UpdateItemSyncStockRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productLevel3UpdateItemSyncStockRequest.setGdnSKU(GDN_SKU);
    productLevel3UpdateItemSyncStockRequest.setSyncStock(true);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_SYNC_STOCK)
            .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID).build();
    this.mockMvc
        .perform(
            post(uri)
                .accept(MediaType.APPLICATION_JSON_VALUE)
                .contentType(MediaType.APPLICATION_JSON)
                .content(
                    this.objectMapper.writeValueAsString(productLevel3UpdateItemSyncStockRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.productLevel3Service).updateSyncStockByBusinessPartnerCodeAndGdnSku(
        ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE, GDN_SKU,
        ProductLevel3ControllerTest.SYNC_STOCK);
  }

  @Test
  public void updateItemSyncStock_checkArg_businessPartnerCode() throws Exception {
    ProductLevel3UpdateItemSyncStockRequest productLevel3UpdateItemSyncStockRequest =
        new ProductLevel3UpdateItemSyncStockRequest();
    productLevel3UpdateItemSyncStockRequest.setGdnSKU(GDN_SKU);
    productLevel3UpdateItemSyncStockRequest.setSyncStock(true);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_SYNC_STOCK)
            .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID).build();
    this.mockMvc
        .perform(
            post(uri)
                .accept(MediaType.APPLICATION_JSON_VALUE)
                .contentType(MediaType.APPLICATION_JSON)
                .content(
                    this.objectMapper.writeValueAsString(productLevel3UpdateItemSyncStockRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void updateItemSyncStock_checkArg_syncStock() throws Exception {
    ProductLevel3UpdateItemSyncStockRequest productLevel3UpdateItemSyncStockRequest =
        new ProductLevel3UpdateItemSyncStockRequest();
    productLevel3UpdateItemSyncStockRequest.setGdnSKU(GDN_SKU);
    productLevel3UpdateItemSyncStockRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_SYNC_STOCK_BUSINESS_PARTNER)
            .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID).build();
    try {
      this.mockMvc.perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
              .content(this.objectMapper.writeValueAsString(productLevel3UpdateItemSyncStockRequest)))
          .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ProductLevel3ControllerErrorMessage.SYNC_STOCK_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void updateItemSyncStock_checkArg_gdnSKU() throws Exception {
    ProductLevel3UpdateItemSyncStockRequest productLevel3UpdateItemSyncStockRequest =
        new ProductLevel3UpdateItemSyncStockRequest();
    productLevel3UpdateItemSyncStockRequest.setSyncStock(true);
    productLevel3UpdateItemSyncStockRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_SYNC_STOCK)
            .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID).build();
    this.mockMvc
        .perform(
            post(uri)
                .accept(MediaType.APPLICATION_JSON_VALUE)
                .contentType(MediaType.APPLICATION_JSON)
                .content(
                    this.objectMapper.writeValueAsString(productLevel3UpdateItemSyncStockRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

  }

  @Test
  public void updateItemSyncStock_failed() throws Exception {
    ProductLevel3UpdateItemSyncStockRequest productLevel3UpdateItemSyncStockRequest =
        new ProductLevel3UpdateItemSyncStockRequest();
    productLevel3UpdateItemSyncStockRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productLevel3UpdateItemSyncStockRequest.setGdnSKU(GDN_SKU);
    productLevel3UpdateItemSyncStockRequest.setSyncStock(true);
    doThrow(new RuntimeException()).when(this.productLevel3Service)
        .updateSyncStockByBusinessPartnerCodeAndGdnSku(
            ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE, GDN_SKU,
            ProductLevel3ControllerTest.SYNC_STOCK);
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.UPDATE_ITEM_SYNC_STOCK)
            .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID).build();
    this.mockMvc
        .perform(
            post(uri)
                .accept(MediaType.APPLICATION_JSON_VALUE)
                .contentType(MediaType.APPLICATION_JSON)
                .content(
                    this.objectMapper.writeValueAsString(productLevel3UpdateItemSyncStockRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.productLevel3Service).updateSyncStockByBusinessPartnerCodeAndGdnSku(
        ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE, GDN_SKU,
        ProductLevel3ControllerTest.SYNC_STOCK);
  }

  @Test
  public void filterSummary_Succeed() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FILTER_SUMMARY)
            .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
            .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
            .addParameter("clientId", CLIENT_ID)
            .addParameter("businessPartnerCode", BUSINESS_PARTNER_CODE).build();
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    request.setItemCode(ITEM_CODE);
    ProductLevel3SummaryFilter filter = new ProductLevel3SummaryFilter();
    filter.setItemCode(ITEM_CODE);
    Page<ProductLevel3Summary> pageOfProductLevel3Summary = generateProductLevel3Summary();
    Mockito.when(
        requestConverter.convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(Mockito
            .any(ProductLevel3SummaryRequest.class))).thenReturn(filter);
    for (ProductLevel3Summary productLevel3Summary : pageOfProductLevel3Summary.getContent()) {
      ProductLevel3SummaryResponse response = new ProductLevel3SummaryResponse();
      BeanUtils.copyProperties(productLevel3Summary, response);
      Mockito.when(
          responseConverter
              .convertProductLevel3SummaryToProductLevel3SummaryResponse(productLevel3Summary))
          .thenReturn(response);
    }
    Mockito.when(
        productLevel3Service.findSummaryByFilter(Mockito.eq(filter), Mockito.any(Pageable.class),
            Mockito.any(SortOrder.class))).thenReturn(pageOfProductLevel3Summary);
    this.mockMvc
        .perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.content[0].nonDistributionAvailable", equalTo(20)))
        .andExpect(jsonPath("$.content[0].nonDistributionReserved", equalTo(10)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(requestConverter)
        .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(productLevel3SummaryRequestCaptor.capture());
    Mockito.verify(productLevel3Service).findSummaryByFilter(Mockito.eq(filter),
        Mockito.any(Pageable.class), Mockito.any(SortOrder.class));
    Mockito
        .verify(responseConverter, Mockito.times(pageOfProductLevel3Summary.getContent().size()))
        .convertProductLevel3SummaryToProductLevel3SummaryResponse(
            Mockito.any(ProductLevel3Summary.class));

    Assertions.assertEquals(ITEM_CODE, productLevel3SummaryRequestCaptor.getValue().getItemCode());
  }

  @Test
  public void filterSummary_Error_EmptyBusinessPartnerCode() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.FILTER_SUMMARY)
            .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
            .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
            .addParameter("clientId", CLIENT_ID).addParameter("businessPartnerCode", "").build();
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    try { this.mockMvc.perform(
            MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request))).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
    }
  }

  @Test
  public void filterSummaryMinified_Succeed() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_SUMMARY_MINIFIED)
            .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
            .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
            .addParameter("clientId", CLIENT_ID)
            .addParameter("businessPartnerCode", BUSINESS_PARTNER_CODE).build();
    ProductLevel3SummaryMinifiedRequest request = new ProductLevel3SummaryMinifiedRequest();
    ProductLevel3SummaryFilter filter = new ProductLevel3SummaryFilter();
    Page<ProductLevel3SummaryMinified> pageOfProductLevel3Summary =
        generateProductLevel3SummaryMinified();
    Mockito.when(
        requestConverter
            .convertProductLevel3SummaryMinifiedRequestToProductLevel3SummaryFilter(Mockito
                .any(ProductLevel3SummaryMinifiedRequest.class))).thenReturn(filter);
    for (ProductLevel3SummaryMinified productLevel3Summary : pageOfProductLevel3Summary
        .getContent()) {
      ProductLevel3SummaryMinifiedResponse response = new ProductLevel3SummaryMinifiedResponse();
      BeanUtils.copyProperties(productLevel3Summary, response);
      Mockito
          .when(
              responseConverter
                  .convertProductLevel3SummaryToProductLevel3SummaryMinifiedResponse(productLevel3Summary))
          .thenReturn(response);
    }
    Mockito.when(
        productLevel3Service.findSummaryMinifiedByFilter(Mockito.eq(filter),
            Mockito.any(Pageable.class), Mockito.any(SortOrder.class))).thenReturn(
        pageOfProductLevel3Summary);
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(request))).andExpect(
        MockMvcResultMatchers.status().isOk());
    Mockito.verify(requestConverter)
        .convertProductLevel3SummaryMinifiedRequestToProductLevel3SummaryFilter(
            Mockito.any(ProductLevel3SummaryMinifiedRequest.class));
    Mockito.verify(productLevel3Service).findSummaryMinifiedByFilter(Mockito.eq(filter),
        Mockito.any(Pageable.class), Mockito.any(SortOrder.class));
    Mockito
        .verify(responseConverter, Mockito.times(pageOfProductLevel3Summary.getContent().size()))
        .convertProductLevel3SummaryToProductLevel3SummaryMinifiedResponse(
            Mockito.any(ProductLevel3SummaryMinified.class));
  }

  @Test
  public void filterSummaryMinified_Error_EmptyBusinessPartnerCode() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH
                    + ProductLevel3ControllerPath.FILTER_SUMMARY_MINIFIED)
            .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
            .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
            .addParameter("clientId", CLIENT_ID).addParameter("businessPartnerCode", "").build();
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                  .content(objectMapper.writeValueAsString(request))).andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
    }
  }

  @Test
  public void getImageBundleTest() throws JsonProcessingException, Exception {
    URI uri = new URIBuilder()
        .setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.ITEM_IMAGE_BUNDLE)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID).build();
    ProductLevel3GdnSkuListRequest request = new ProductLevel3GdnSkuListRequest();
    request.setGdnSkus(Arrays.asList(GDN_SKU));
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.productLevel3Service).findImageBundleByFilter(
        Mockito.any(ItemSummaryRequest.class), Mockito.any(Pageable.class),
        Mockito.any(SortOrder.class));
  }

  @Test
  public void getImageBundleTest_EmptyRequest() throws JsonProcessingException, Exception {
    URI uri = new URIBuilder()
        .setPath(
            ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.ITEM_IMAGE_BUNDLE)
        .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
        .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
        .addParameter("clientId", CLIENT_ID).build();
    ProductLevel3GdnSkuListRequest request = new ProductLevel3GdnSkuListRequest();
    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                  .content(objectMapper.writeValueAsString(request))).andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(ProductLevel3ControllerErrorMessage.REQUEST_MUST_NOT_BE_EMPTY));
    }
  }

  @Test
  public void updateResignBusinessPartnerItems() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH
            + ProductLevel3ControllerPath.UPDATE_RESIGN_BUSINESS_PARTNER_ITEMS)
        .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
        .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
        .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
        .addParameter("username", ProductLevel3ControllerTest.USERNAME)
        .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID)
        .addParameter("businessPartnerCode", ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE)
        .build();
    this.mockMvc
        .perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.productLevel3Service)
        .updateResignBusinessPartnerItems(STORE_ID, ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE);
  }

  @Test
  public void updateResignBusinessPartnerItems_emptyBPCODE() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH
            + ProductLevel3ControllerPath.UPDATE_RESIGN_BUSINESS_PARTNER_ITEMS)
        .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
        .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
        .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
        .addParameter("username", ProductLevel3ControllerTest.USERNAME)
        .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID)
        .addParameter("businessPartnerCode", "")
        .build();
    try { this.mockMvc.perform(
            post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void searchItemTest_emptyBPCODE() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.SEARCH_ITEM)
            .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID)
            .addParameter("businessPartnerCode", "").build();
    ProductLevel3ItemSearchRequest request = new ProductLevel3ItemSearchRequest();
    try {
      this.mockMvc.perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)
              .content(this.objectMapper.writeValueAsString(request))).andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
    }
  }

  @Test
  public void searchItemTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.SEARCH_ITEM)
            .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
            .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
            .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
            .addParameter("username", ProductLevel3ControllerTest.USERNAME)
            .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID)
            .addParameter("businessPartnerCode", ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE)
            .build();
    ProductLevel3ItemSearchRequest level3ItemSearchRequest = new ProductLevel3ItemSearchRequest();
    ProductLevel3ItemSearch productLevel3ItemSearch = new ProductLevel3ItemSearch();
    ProductLevel3Item productLevel3Item = new ProductLevel3Item();
    List<ProductLevel3Item> productLevel3ItemList = new ArrayList<>();
    productLevel3ItemList.add(productLevel3Item);
    Page<ProductLevel3Item> productLevel3ItemPage = new PageImpl<>(productLevel3ItemList);
    Mockito.when(this.requestConverter
        .convertProductLevel3ItemSearchRequestToProductLevel3ItemSearch(level3ItemSearchRequest))
        .thenReturn(productLevel3ItemSearch);
    Mockito
        .when(this.productLevel3Service.findItemBySearch(Mockito.eq(productLevel3ItemSearch),
            Mockito.any(Pageable.class), Mockito.any(SortOrder.class)))
        .thenReturn(productLevel3ItemPage);
    Mockito
        .when(this.responseConverter.convertProductLevel3ItemToProductLevel3ItemSearchResponse(
            Mockito.any(ProductLevel3Item.class)))
        .thenReturn(new ProductLevel3ItemSearchResponse());
    this.mockMvc
        .perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON)
            .content(this.objectMapper.writeValueAsString(level3ItemSearchRequest)))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.requestConverter)
        .convertProductLevel3ItemSearchRequestToProductLevel3ItemSearch(level3ItemSearchRequest);
    Mockito.verify(this.productLevel3Service).findItemBySearch(Mockito.eq(productLevel3ItemSearch),
        Mockito.any(Pageable.class), Mockito.any(SortOrder.class));
    Mockito.verify(this.responseConverter)
        .convertProductLevel3ItemToProductLevel3ItemSearchResponse(
            Mockito.any(ProductLevel3Item.class));
  }

  @Test
  public void productAssignmentTest() throws Exception {
    Mockito.doNothing().when(productServiceWrapper)
        .updateProductAssignmentStatus(STORE_ID, PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH
            + ProductLevel3ControllerPath.UPDATE_PRODUCT_ASSIGNMENT)
        .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
        .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
        .addParameter("assignedTo", ASSIGNED_TO)
        .addParameter("assignedBy", ASSIGNED_BY)
        .addParameter("productCode", PRODUCT_CODE)
        .build();
    this.mockMvc.perform(
        MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productServiceWrapper)
        .updateProductAssignmentStatus(STORE_ID, PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY);
  }

  @Test
  public void productAssignmentExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(productServiceWrapper)
        .updateProductAssignmentStatus(STORE_ID, PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY);
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH
            + ProductLevel3ControllerPath.UPDATE_PRODUCT_ASSIGNMENT)
        .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
        .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
        .addParameter("assignedTo", ASSIGNED_TO)
        .addParameter("assignedBy", ASSIGNED_BY)
        .addParameter("productCode", PRODUCT_CODE)
        .build();
    this.mockMvc.perform(
        MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productServiceWrapper)
        .updateProductAssignmentStatus(STORE_ID, PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY);
  }

  @Test
  public void productAssignmentProductCodeEmptyTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH
            + ProductLevel3ControllerPath.UPDATE_PRODUCT_ASSIGNMENT)
        .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
        .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
        .addParameter("assignedTo", ASSIGNED_TO)
        .addParameter("assignedBy", ASSIGNED_BY)
        .addParameter("productCode", StringUtils.EMPTY)
        .build();
    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(ProductLevel3ControllerErrorMessage.ERROR_PRODUCT_CODE_EMPTY));
    }
  }

  @Test
  public void productAssignmentStoreIdEmptyTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH
            + ProductLevel3ControllerPath.UPDATE_PRODUCT_ASSIGNMENT)
        .addParameter("storeId", StringUtils.EMPTY)
        .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
        .addParameter("assignedTo", ASSIGNED_TO)
        .addParameter("assignedBy", ASSIGNED_BY)
        .addParameter("productCode", PRODUCT_CODE)
        .build();
    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ProductLevel3ControllerErrorMessage.ERROR_STORE_ID_EMPTY));
    }
  }

  @Test
  public void productAssignmentAssignedByEmptyTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH
            + ProductLevel3ControllerPath.UPDATE_PRODUCT_ASSIGNMENT)
        .addParameter("storeId", StringUtils.EMPTY)
        .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
        .addParameter("assignedTo", ASSIGNED_TO)
        .addParameter("assignedBy", ASSIGNED_BY)
        .addParameter("productCode", PRODUCT_CODE)
        .build();
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ProductLevel3ControllerErrorMessage.ERROR_STORE_ID_EMPTY));
    }
  }

  @Test
  public void checkAvailableStockTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.CHECK_AVAILABLE_STOCK)
        .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
        .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
        .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID)
        .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
        .addParameter("businessPartnerCode", ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE).build();
    Mockito.when(this.productLevel3Service.checkAvailableStock(ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE))
        .thenReturn(false);
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.productLevel3Service).checkAvailableStock(ProductLevel3ControllerTest.BUSINESS_PARTNER_CODE);
  }

  @Test
  public void checkAvailableStockBusinessPartnerCodeEmptyTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.CHECK_AVAILABLE_STOCK)
        .addParameter("storeId", ProductLevel3ControllerTest.STORE_ID)
        .addParameter("channelId", ProductLevel3ControllerTest.CHANNEL_ID)
        .addParameter("clientId", ProductLevel3ControllerTest.CLIENT_ID)
        .addParameter("requestId", ProductLevel3ControllerTest.REQUEST_ID)
        .addParameter("businessPartnerCode", StringUtils.EMPTY).build();
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK));
    }
  }

  @Test
  public void productsAvailableToCopy_Succeed() throws Exception {
    URI uri = new URIBuilder()
      .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.COPY_PRODUCTS_FILTER_SUMMARY)
      .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
      .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
      .addParameter("clientId", CLIENT_ID)
      .addParameter("businessPartnerCode", BUSINESS_PARTNER_CODE)
      .addParameter("linkedPartnerCode", LINKED_BUSINESS_PARTNER_CODE)
      .build();

    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    request.setItemCode(ITEM_CODE);

    ProductLevel3SummaryFilter filter = new ProductLevel3SummaryFilter();
    filter.setStoreId(STORE_ID);
    filter.setItemCode(ITEM_CODE);
    filter.setBusinessPartnerCode(LINKED_BUSINESS_PARTNER_CODE);

    Page<ProductLevel3Summary> pageOfProductLevel3Summary = generateProductLevel3Summary();
    Page<AvailableToCopyProductDetailsResponse> productsAvailableToBeCopied = generateProductsAvailableToBeCopiedResponse();

    Mockito.when(requestConverter
      .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(Mockito.any(ProductLevel3SummaryRequest.class)))
      .thenReturn(filter);

    Mockito
      .when(productLevel3Service.productsAvailableToBeCopied(eq(filter), eq(BUSINESS_PARTNER_CODE), any(Pageable.class)))
      .thenReturn(productsAvailableToBeCopied);

    this.mockMvc
      .perform(MockMvcRequestBuilders.post(uri)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request)))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.content[0].productSku", equalTo("TOQ-19301-00001")))
      .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(requestConverter)
      .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(productLevel3SummaryRequestCaptor.capture());

    Mockito
      .verify(productLevel3Service)
      .productsAvailableToBeCopied(eq(filter), eq(BUSINESS_PARTNER_CODE), any(Pageable.class));

    Assertions.assertEquals(ITEM_CODE, productLevel3SummaryRequestCaptor.getValue().getItemCode());
  }

  @Test
  public void productsAvailableToCopy_whenEmptyBusinessPartnerCode() throws Exception {
    URI uri = new URIBuilder()
      .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.COPY_PRODUCTS_FILTER_SUMMARY)
      .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
      .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
      .addParameter("clientId", CLIENT_ID)
      .addParameter("businessPartnerCode", "")
      .addParameter("linkedPartnerCode", LINKED_BUSINESS_PARTNER_CODE)
      .build();

    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                  .content(objectMapper.writeValueAsString(new ProductLevel3SummaryRequest())))
          .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
    }
  }

  @Test
  public void productsAvailableToCopy_whenEmptyLinkedBusinessPartnerCode() throws Exception {
    URI uri = new URIBuilder()
      .setPath(ProductLevel3ControllerPath.BASE_PATH + ProductLevel3ControllerPath.COPY_PRODUCTS_FILTER_SUMMARY)
      .addParameter("storeId", STORE_ID).addParameter("channelId", CHANNEL_ID)
      .addParameter("requestId", REQUEST_ID).addParameter("username", USERNAME)
      .addParameter("clientId", CLIENT_ID)
      .addParameter("businessPartnerCode", BUSINESS_PARTNER_CODE)
      .addParameter("linkedPartnerCode", "")
      .build();

    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                  .content(objectMapper.writeValueAsString(new ProductLevel3SummaryRequest())))
          .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY));
    }
  }

  private Page<AvailableToCopyProductDetailsResponse> generateProductsAvailableToBeCopiedResponse() {
    Pageable pageRequest = PageRequest.of(0, 10);
    AvailableToCopyItemDetailsResponse availableToCopyItemDetailsResponse = AvailableToCopyItemDetailsResponse.builder()
      .itemName("item-name-1")
      .itemSku("TOQ-19301-00001-00001")
      .build();
    AvailableToCopyProductDetailsResponse availableToCopyProductDetailsResponse = AvailableToCopyProductDetailsResponse
      .builder()
      .categoryName("category-name-1")
      .totalItemSkuCount(1)
      .productSku("TOQ-19301-00001")
      .productName("product-name-1")
      .status("IN_PROGRESS")
      .itemDetails(Arrays.asList(availableToCopyItemDetailsResponse))
      .build();
    return new PageImpl<>(Arrays.asList(availableToCopyProductDetailsResponse), pageRequest, 1);
  }
}
