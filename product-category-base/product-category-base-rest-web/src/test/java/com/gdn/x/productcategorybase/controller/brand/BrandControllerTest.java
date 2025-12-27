package com.gdn.x.productcategorybase.controller.brand;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

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
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
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

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.BrandControllerErrorMessage;
import com.gdn.x.productcategorybase.BrandControllerPath;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.BrandDTO;
import com.gdn.x.productcategorybase.dto.BrandSummaryFilterDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandNamesRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandPredefinedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandSummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandRequest;
import com.gdn.x.productcategorybase.dto.brand.UndeleteBrandRequest;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.solr.SolrBrandModel;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;
import com.gdn.x.productcategorybase.service.brand.BrandService;
import com.gdn.x.productcategorybase.service.brand.BrandWipService;

public class BrandControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "TEST";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final String DEFAULT_BRAND_CODE = "BRD-00001";
  private static final String DEFAULT_DELETION_REASON = "brandDeleteReason";
  private static final String DEFAULT_BRAND_NAME = "Blibli.com";
  private static final String DEFAULT_BRAND_NAME_LOWER = "blibli.com";
  private static final String INVALID_BRAND_NAME = "Blibli.comå";
  private static final String DEFAULT_BRAND_DESCRIPTION = "Brand description";
  private static final String DEFAULT_BRAND_INFO = "Brand info";
  private static final String DEFAULT_BRAND_LOGO_PATH = "blibli-com-logo.jpg";
  private static final String DEFAULT_BRAND_REQUEST_CODE = "brandRequestCode";
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);
  private static final VerificationMode CALLED_THREE_TIME = Mockito.times(3);
  private static final String DEFAULT_BRAND_LOGO = "brand_logo";
  private static final String DEFAULT_PROFILE_BANNER = "profile_banner";
  private static final String FILTER_BRAND_CODE_PATH = "/filter/BRD-00001/status/APPROVED";
  private static final String PROTECTED_BRAND = "protected_brand";

  @Mock
  private BrandService brandService;

  @Mock
  private BrandWipService brandWipService;

  @Mock
  private RedisTemplate<String, BrandResponse> brandRedisTemplate;
  
  @Mock
  private ValueOperations<String, BrandResponse> valueOperations;

  @InjectMocks
  private BrandController brandController;

  @Mock
  private SolrBrandRepository solrBrandRepository;

  @Captor
  private ArgumentCaptor<BrandResponse> brandResponseArgumentCaptor;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc =
        MockMvcBuilders
            .standaloneSetup(this.brandController)
            .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
                new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
                new MappingJackson2HttpMessageConverter()).build();
    this.objectMapper = new ObjectMapper(new JsonFactory());
    Brand brand = this.generateBrand();
    Page<Brand> brands = this.generateBrands();
    when(this.brandService.create(Mockito.any(Brand.class))).thenReturn(BrandControllerTest.DEFAULT_BRAND_CODE);
    when(this.brandService.findByBrandCode(Mockito.anyString())).thenReturn(brand);
    when(this.brandService.findSummaryByName(Mockito.anyString(),
            Mockito.any(Pageable.class))).thenReturn(brands);
    when(this.brandService.findByBrandName(Mockito.anyString(), Mockito.anyBoolean())).thenReturn(brand);
    when(this.brandRedisTemplate.hasKey(Mockito.anyString())).thenReturn(true);
    when(this.brandService.delete(Mockito.anyString(), Mockito.anyString())).thenReturn(BrandControllerTest.DEFAULT_BRAND_NAME);
    when(this.brandRedisTemplate.opsForValue()).thenReturn(this.valueOperations);
    when(this.valueOperations.get(Mockito.any())).thenReturn(null);
    when(this.brandService.undelete(Mockito.any(Brand.class))).thenReturn(
        brand);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.brandService);
    Mockito.verifyNoMoreInteractions(this.brandRedisTemplate);
    Mockito.verifyNoMoreInteractions(this.valueOperations);
    Mockito.verifyNoMoreInteractions(this.solrBrandRepository);
  }

  private Brand generateBrand() throws Exception {
    Brand brand = new Brand();
    brand.setBrandCode(BrandControllerTest.DEFAULT_BRAND_CODE);
    brand.setBrandName(BrandControllerTest.DEFAULT_BRAND_NAME);
    brand.setBrandDescription(BrandControllerTest.DEFAULT_BRAND_DESCRIPTION.getBytes());
    brand.setBrandLogoPath(BrandControllerTest.DEFAULT_BRAND_LOGO_PATH);
    brand.setValidBrand(true);
    return brand;
  }

  private Page<Brand> generateBrands() throws Exception {
    List<Brand> brands = new ArrayList<Brand>();
    brands.add(this.generateBrand());
    return new PageImpl<Brand>(brands);
  }

  private CreateBrandRequest generateCreateBrandRequest() throws Exception {
    CreateBrandRequest request = new CreateBrandRequest();
    request.setBrandName(BrandControllerTest.DEFAULT_BRAND_NAME);
    request.setBrandDescription(BrandControllerTest.DEFAULT_BRAND_DESCRIPTION);
    request.setBrandLogoPath(BrandControllerTest.DEFAULT_BRAND_LOGO_PATH);
    return request;
  }

  private CreateBrandRequest generateInvalidBrandRequest() throws Exception {
    CreateBrandRequest request = new CreateBrandRequest();
    request.setBrandName(BrandControllerTest.INVALID_BRAND_NAME);
    request.setBrandDescription(BrandControllerTest.DEFAULT_BRAND_DESCRIPTION);
    request.setBrandLogoPath(BrandControllerTest.DEFAULT_BRAND_LOGO_PATH);
    return request;
  }

  private BrandSummaryRequest generateBrandSummaryRequest() {
    BrandSummaryRequest request = new BrandSummaryRequest();
    request.setBrandName(BrandControllerTest.DEFAULT_BRAND_NAME);
    return request;
  }

  private UpdateBrandRequest generateUpdateBrandRequest() throws Exception {
    UpdateBrandRequest request = new UpdateBrandRequest();
    request.setBrandCode(BrandControllerTest.DEFAULT_BRAND_CODE);
    request.setBrandName(BrandControllerTest.DEFAULT_BRAND_NAME);
    request.setBrandDescription(BrandControllerTest.DEFAULT_BRAND_DESCRIPTION);
    request.setBrandLogoPath(BrandControllerTest.DEFAULT_BRAND_LOGO_PATH);
    request.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    request.setBrandLogo(DEFAULT_BRAND_LOGO);
    request.setProfileBanner(DEFAULT_PROFILE_BANNER);
    request.setSkuCreationAllowedForAllSellers(Boolean.TRUE);
    request.setValidBrand(true);
    return request;
  }

  private UpdateBrandRequest generateUpdateBrandRequestwithSkuCreationAllowedForAllSellersNull() throws Exception {
    UpdateBrandRequest request = new UpdateBrandRequest();
    request.setBrandCode(BrandControllerTest.DEFAULT_BRAND_CODE);
    request.setBrandName(BrandControllerTest.DEFAULT_BRAND_NAME);
    request.setBrandDescription(BrandControllerTest.DEFAULT_BRAND_DESCRIPTION);
    request.setBrandLogoPath(BrandControllerTest.DEFAULT_BRAND_LOGO_PATH);
    request.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    request.setBrandLogo(DEFAULT_BRAND_LOGO);
    request.setProfileBanner(DEFAULT_PROFILE_BANNER);
    request.setSkuCreationAllowedForAllSellers(null);
    request.setValidBrand(true);
    return request;
  }
  
  private UndeleteBrandRequest generateUndeleteBrandRequest() throws Exception {
    UndeleteBrandRequest request = new UndeleteBrandRequest();
    request.setBrandName(BrandControllerTest.DEFAULT_BRAND_NAME);
    request.setBrandDescription(BrandControllerTest.DEFAULT_BRAND_DESCRIPTION);
    request.setBrandLogoPath(BrandControllerTest.DEFAULT_BRAND_LOGO_PATH);
    return request;
  }

  @Test
  public void createTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.CREATE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(this.generateCreateBrandRequest());
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).create(Mockito.any(Brand.class));
  }

  @Test
  public void createWithBrandNameExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.CREATE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    CreateBrandRequest request = this.generateCreateBrandRequest();
    request.setBrandName(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)));
    } catch (Exception e) {
      Mockito.verify(this.brandService, BrandControllerTest.NEVER_CALLED).create(Mockito.any(Brand.class));
    }
  }

  @Test
  public void createWithInValidBrandNameExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.CREATE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    CreateBrandRequest request = this.generateInvalidBrandRequest();
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)));
    } catch (Exception e) {
      Assertions.assertEquals(true, e.getMessage().contains(BrandControllerErrorMessage.INVALID_BRAND_NAME_CHARACTERS));
    }
  }

  @Test
  public void createWithBrandDescriptionExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.CREATE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    CreateBrandRequest request = this.generateCreateBrandRequest();
    request.setBrandDescription(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)));
    } catch (Exception e) {
      Mockito.verify(this.brandService, BrandControllerTest.NEVER_CALLED).create(Mockito.any(Brand.class));
    }
  }

  @Test
  public void filterByBrandCodeTest() throws Exception {
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setValidBrand(true);
    when(brandService.getBrandResponseByBrandCode(BrandControllerTest.DEFAULT_STORE_ID, BrandControllerTest.DEFAULT_BRAND_CODE)).thenReturn(brandResponse);
    URI uri = new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.FILTER_BRAND_CODE)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME)
        .addParameter("brandCode", BrandControllerTest.DEFAULT_BRAND_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.value.validBrand", equalTo(true)));
    Mockito.verify(this.brandService).getBrandResponseByBrandCode(BrandControllerTest.DEFAULT_STORE_ID, BrandControllerTest.DEFAULT_BRAND_CODE);
  }

  
  @Test
  public void filterByBrandCodeWithNullBrandLogoResultTest() throws Exception {
    Brand brand = this.generateBrand();
    brand.setBrandLogoPath(null);
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setValidBrand(true);
    when(brandService.getBrandResponseByBrandCode(BrandControllerTest.DEFAULT_STORE_ID, BrandControllerTest.DEFAULT_BRAND_CODE)).thenReturn(brandResponse);
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.FILTER_BRAND_CODE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME)
            .addParameter("brandCode", BrandControllerTest.DEFAULT_BRAND_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).getBrandResponseByBrandCode(BrandControllerTest.DEFAULT_STORE_ID, BrandControllerTest.DEFAULT_BRAND_CODE);

  }

  @Test
  public void filterByBrandNameTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.FILTER_BRAND_NAME)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME)
            .addParameter("brandName", BrandControllerTest.DEFAULT_BRAND_NAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).getBrandResponseByBrandName(BrandControllerTest.DEFAULT_STORE_ID,
      BrandControllerTest.DEFAULT_BRAND_NAME, false, false);  }
  
  @Test
  public void filterByBrandNameWithBrandCacheTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.FILTER_BRAND_NAME)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME)
            .addParameter("brandName", BrandControllerTest.DEFAULT_BRAND_NAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).getBrandResponseByBrandName(BrandControllerTest.DEFAULT_STORE_ID,
      BrandControllerTest.DEFAULT_BRAND_NAME, false, false);
  }
  
  @Test
  public void filterByBrandNameWithNullBrandTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.FILTER_BRAND_NAME)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME)
            .addParameter("brandName", BrandControllerTest.DEFAULT_BRAND_NAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).getBrandResponseByBrandName(BrandControllerTest.DEFAULT_STORE_ID,
      BrandControllerTest.DEFAULT_BRAND_NAME, false, false);
  }

  @Test
  public void filterByBrandNameWithBrandWipTest() throws Exception {
    BrandResponse brandResponse = new BrandResponse();
     when(brandService.getBrandResponseByBrandName(BrandControllerTest.DEFAULT_STORE_ID,
      BrandControllerTest.DEFAULT_BRAND_NAME, false, true)).thenReturn(brandResponse);
    URI uri = new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.FILTER_BRAND_NAME)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME)
        .addParameter("brandName", BrandControllerTest.DEFAULT_BRAND_NAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).getBrandResponseByBrandName(BrandControllerTest.DEFAULT_STORE_ID,
      BrandControllerTest.DEFAULT_BRAND_NAME, false, false);
  }

  @Test
  public void filterByBrandNameFromOnlyActiveBrandsTest() throws Exception {
    BrandResponse brandResponse = new BrandResponse();
    when(brandService.getBrandResponseByBrandName(BrandControllerTest.DEFAULT_STORE_ID,
      BrandControllerTest.DEFAULT_BRAND_NAME, false, true)).thenReturn(brandResponse);
    URI uri = new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.FILTER_BRAND_NAME)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME)
        .addParameter("brandName", BrandControllerTest.DEFAULT_BRAND_NAME)
        .addParameter("activeBrandsOnly", String.valueOf(true)).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).getBrandResponseByBrandName(BrandControllerTest.DEFAULT_STORE_ID,
      BrandControllerTest.DEFAULT_BRAND_NAME, false, true);

  }

  @Test
  public void filterByBrandCodeWithNullBrandTest() throws Exception {
    when(brandService.getBrandResponseByBrandCode(BrandControllerTest.DEFAULT_STORE_ID,
      BrandControllerTest.DEFAULT_BRAND_CODE)).thenReturn(null);
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.FILTER_BRAND_CODE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME)
            .addParameter("brandCode", BrandControllerTest.DEFAULT_BRAND_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).getBrandResponseByBrandCode(BrandControllerTest.DEFAULT_STORE_ID, BrandControllerTest.DEFAULT_BRAND_CODE);
  }

  @Test
  public void filterByBrandCodeWithNullBrandDescriptionTest() throws Exception {
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setValidBrand(true);
    brandResponse.setBrandDescription(null);
    when(brandService.getBrandResponseByBrandCode(BrandControllerTest.DEFAULT_STORE_ID, BrandControllerTest.DEFAULT_BRAND_CODE)).thenReturn(brandResponse);
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.FILTER_BRAND_CODE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME)
            .addParameter("brandCode", BrandControllerTest.DEFAULT_BRAND_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).getBrandResponseByBrandCode(BrandControllerTest.DEFAULT_STORE_ID, BrandControllerTest.DEFAULT_BRAND_CODE);
  }

  @Test
  public void filterSummaryTest() throws Exception {
    Brand brand = new Brand();
    brand.setBrandName(DEFAULT_BRAND_NAME);
    Page<Brand> brands = new PageImpl<>(Arrays.asList(brand), PageRequest.of(0, 10), 1);
    BrandSummaryFilterDTO filter =
        BrandSummaryFilterDTO.builder()
            .brandName(DEFAULT_BRAND_NAME)
            .pageable(PageRequest.of(0, 10))
            .build();
    when(brandService.findSummaryByFilter(filter, DEFAULT_STORE_ID)).thenReturn(brands);
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.FILTER_SUMMARY)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(this.generateBrandSummaryRequest());
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", hasSize(1)))
        .andExpect(jsonPath("$.content[0].brandName", equalTo(DEFAULT_BRAND_NAME)));
    verify(brandService).findSummaryByFilter(filter, DEFAULT_STORE_ID);
  }

  @Test
  public void filterSummaryWithoutBrandNameTest() throws Exception {
    Page<Brand> brands = new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 10), 0);
    BrandSummaryFilterDTO filter =
        BrandSummaryFilterDTO.builder()
            .pageable(PageRequest.of(0, 10))
            .build();
    when(brandService.findSummaryByFilter(filter, DEFAULT_STORE_ID)).thenReturn(brands);
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.FILTER_SUMMARY)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    BrandSummaryRequest request = this.generateBrandSummaryRequest();
    request.setBrandName(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", hasSize(0)));
    verify(brandService).findSummaryByFilter(filter, DEFAULT_STORE_ID);
  }
  
  @Test
  public void filterSummaryByNameTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.FILTER_SUMMARY_ORDER_BY_NAME)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("brandName", BrandControllerTest.DEFAULT_BRAND_NAME)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).findSummaryByName(Mockito.anyString(), Mockito.any(Pageable.class));
  }

  @Test
  public void filterByBrandCodeAndStatusTest() throws Exception {
    Mockito.when(brandService
        .getBrandPredefinedValueByCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, Constants.APPROVED_STATUS))
        .thenReturn(new BrandPredefinedAttributeValueResponse());
    URI uri = new URIBuilder().setPath(BrandControllerPath.BASE_PATH + FILTER_BRAND_CODE_PATH)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService)
        .getBrandPredefinedValueByCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, Constants.APPROVED_STATUS);
  }

  @Test
  public void filterByBrandCodeAndStatusExceptionTest() throws Exception {
    Mockito.when(brandService
        .getBrandPredefinedValueByCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, Constants.APPROVED_STATUS))
        .thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder().setPath(BrandControllerPath.BASE_PATH + FILTER_BRAND_CODE_PATH)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.brandService)
        .getBrandPredefinedValueByCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, Constants.APPROVED_STATUS);
  }
  
  @Test
  public void updateTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.UPDATE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(this.generateUpdateBrandRequest());
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService)
        .update(Mockito.any(Brand.class), Mockito.eq(DEFAULT_BRAND_REQUEST_CODE), Mockito.eq(DEFAULT_BRAND_LOGO),
            Mockito.eq(DEFAULT_PROFILE_BANNER), Mockito.eq(DEFAULT_USERNAME), Mockito.eq(true));
    Mockito.verify(brandService)
        .deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME_LOWER, DEFAULT_BRAND_CODE);

  }

  @Test
  public void updateTestwithskuCreationAllowedForAllSellersAsNull() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.UPDATE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(this.generateUpdateBrandRequestwithSkuCreationAllowedForAllSellersNull());
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService)
        .update(Mockito.any(Brand.class), Mockito.eq(DEFAULT_BRAND_REQUEST_CODE), Mockito.eq(DEFAULT_BRAND_LOGO),
            Mockito.eq(DEFAULT_PROFILE_BANNER), Mockito.eq(DEFAULT_USERNAME), Mockito.eq(null));
    Mockito.verify(brandService)
        .deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME_LOWER, DEFAULT_BRAND_CODE);

  }

  @Test
  public void updateWithoutDeleteBrandCacheTest() throws Exception {
    when(this.brandRedisTemplate.hasKey(Mockito.anyString())).thenReturn(false);
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.UPDATE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(this.generateUpdateBrandRequest());
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService)
        .update(Mockito.any(Brand.class), Mockito.eq(DEFAULT_BRAND_REQUEST_CODE), Mockito.eq(DEFAULT_BRAND_LOGO),
            Mockito.eq(DEFAULT_PROFILE_BANNER), Mockito.eq(DEFAULT_USERNAME), Mockito.eq(true));
    Mockito.verify(brandService)
        .deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME_LOWER, DEFAULT_BRAND_CODE);
  }

  @Test
  public void updateWithBrandCodeExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.UPDATE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    UpdateBrandRequest request = this.generateUpdateBrandRequest();
    request.setBrandCode(null);;
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)));
    } catch (Exception e) {
      Mockito.verify(this.brandService, BrandControllerTest.NEVER_CALLED)
          .update(Mockito.any(Brand.class), Mockito.eq(DEFAULT_BRAND_REQUEST_CODE), Mockito.eq(DEFAULT_BRAND_LOGO),
              Mockito.eq(DEFAULT_PROFILE_BANNER), Mockito.eq(DEFAULT_USERNAME), Mockito.eq(null));
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).hasKey(Mockito.anyString());
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).delete(Mockito.anyString());
    }
  }

  @Test
  public void updateWithBrandNameExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.UPDATE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    UpdateBrandRequest request = this.generateUpdateBrandRequest();
    request.setBrandName(null);;
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)));
    } catch (Exception e) {
      Mockito.verify(this.brandService, BrandControllerTest.NEVER_CALLED)
          .update(Mockito.any(Brand.class), Mockito.eq(DEFAULT_BRAND_REQUEST_CODE), Mockito.eq(DEFAULT_BRAND_LOGO),
              Mockito.eq(DEFAULT_PROFILE_BANNER), Mockito.eq(DEFAULT_USERNAME), Mockito.eq(null));
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).hasKey(Mockito.anyString());
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).delete(Mockito.anyString());
    }
  }

  @Test
  public void updateWithBrandDescriptionExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.UPDATE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    UpdateBrandRequest request = this.generateUpdateBrandRequest();
    request.setBrandDescription(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)));
    } catch (Exception e) {
      Mockito.verify(this.brandService, BrandControllerTest.NEVER_CALLED)
          .update(Mockito.any(Brand.class), Mockito.eq(DEFAULT_BRAND_REQUEST_CODE), Mockito.eq(DEFAULT_BRAND_LOGO),
              Mockito.eq(DEFAULT_PROFILE_BANNER), Mockito.eq(DEFAULT_USERNAME), Mockito.eq(null));
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).hasKey(Mockito.anyString());
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).delete(Mockito.anyString());
    }
  }
  
  @Test
  public void deleteTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.DELETE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME)
            .addParameter("brandCode", BrandControllerTest.DEFAULT_BRAND_CODE)
            .addParameter("brandDeletedReason", BrandControllerTest.DEFAULT_DELETION_REASON).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).delete(DEFAULT_BRAND_CODE, DEFAULT_DELETION_REASON);
    Mockito.verify(brandService).deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, DEFAULT_BRAND_CODE);

  }

  @Test
  public void deleteWithBrandCodeApplicationRuntimeExceptionTest() throws Exception {
    Mockito.when(brandService.delete(DEFAULT_BRAND_CODE, DEFAULT_DELETION_REASON))
        .thenThrow(new ApplicationRuntimeException());
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.DELETE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME)
            .addParameter("brandCode", "").build();
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)));
    } catch (ApplicationRuntimeException e) {

    }finally {
      Mockito.verify(this.brandService, BrandControllerTest.NEVER_CALLED)
          .delete(Mockito.anyString(), Mockito.anyString());
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).hasKey(Mockito.anyString());
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).delete(Mockito.anyString());
    }
  }
  
  @Test
  public void deleteWithBrandCodeExceptionTest() throws Exception {
    Mockito.when(brandService.delete(DEFAULT_BRAND_CODE, DEFAULT_DELETION_REASON))
        .thenThrow(new ApplicationException());
    URI uri = new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.DELETE)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).addParameter("brandCode", DEFAULT_BRAND_CODE)
        .addParameter("brandDeletedReason", BrandControllerTest.DEFAULT_DELETION_REASON).build();
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {

    }finally {
      Mockito.verify(this.brandService)
          .delete(Mockito.anyString(), Mockito.anyString());
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).hasKey(Mockito.anyString());
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).delete(Mockito.anyString());
    }
  }
  
  @Test
  public void undeleteTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.UNDELETE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(this.generateUndeleteBrandRequest());
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).undelete(Mockito.any(Brand.class));
    Mockito.verify(solrBrandRepository).addBrandsToBrandCollectionSolr(Mockito.any());
    Mockito.verify(brandService).deleteAllBrandCache(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, DEFAULT_BRAND_CODE);

  }

  @Test
  public void undeleteWithBrandNameExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.UNDELETE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    UndeleteBrandRequest request = this.generateUndeleteBrandRequest();
    request.setBrandName(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)));
    } catch (Exception e) {
      Mockito.verify(this.brandService, BrandControllerTest.NEVER_CALLED).undelete(Mockito.any(Brand.class));
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).hasKey(Mockito.anyString());
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).delete(Mockito.anyString());
    }
  }
  
  @Test
  public void undeleteWithBrandDescriptionExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.UNDELETE)
            .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    UndeleteBrandRequest request = this.generateUndeleteBrandRequest();
    request.setBrandDescription(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(jsonPath("$.success", equalTo(true)));
    } catch (Exception e) {
      Mockito.verify(this.brandService, BrandControllerTest.NEVER_CALLED).undelete(Mockito.any(Brand.class));
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).hasKey(Mockito.anyString());
      Mockito.verify(this.brandRedisTemplate, BrandControllerTest.NEVER_CALLED).delete(Mockito.anyString());
    }
  }

  @Test
  public void getBrandNamesByBrandCodesTest() throws Exception {
    List<BrandDTO> brandDTOS = new ArrayList<>();
    brandDTOS.add(new BrandDTO(DEFAULT_BRAND_CODE, DEFAULT_BRAND_NAME));
    when(brandService.findBrandNamesByBrandCodes(Arrays.asList(DEFAULT_BRAND_CODE)))
        .thenReturn(brandDTOS);
    URI uri = new URIBuilder()
        .setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.GET_BRAND_NAMES_BY_CODES)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    BrandNamesRequest request = new BrandNamesRequest(Arrays.asList(DEFAULT_BRAND_CODE));
    String requestBody = this.objectMapper.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).findBrandNamesByBrandCodes(Arrays.asList(DEFAULT_BRAND_CODE));
  }

  @Test
  public void getBrandNamesByBrandCodesNullTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.GET_BRAND_NAMES_BY_CODES)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    BrandNamesRequest request = new BrandNamesRequest(new ArrayList<String>());
    String requestBody = this.objectMapper.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void getBrandNamesByBrandCodesExceptionTest() throws Exception {
    List<BrandDTO> brandDTOS = new ArrayList<>();
    brandDTOS.add(new BrandDTO(DEFAULT_BRAND_CODE, DEFAULT_BRAND_NAME));
    when(brandService.findBrandNamesByBrandCodes(Arrays.asList(DEFAULT_BRAND_CODE)))
        .thenThrow(Exception.class);
    URI uri = new URIBuilder()
        .setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.GET_BRAND_NAMES_BY_CODES)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    BrandNamesRequest request = new BrandNamesRequest(Arrays.asList(DEFAULT_BRAND_CODE));
    String requestBody = this.objectMapper.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.brandService).findBrandNamesByBrandCodes(Arrays.asList(DEFAULT_BRAND_CODE));
  }


  @Test
  public void getBrandNamesBy501BrandCodesTest() throws Exception {
    List<String> categoryCodes = new ArrayList<>();
    for(int i=0; i< 502; i++) {
      categoryCodes.add(DEFAULT_BRAND_CODE);
    }
    URI uri = new URIBuilder()
        .setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.GET_BRAND_NAMES_BY_CODES)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    BrandNamesRequest request = new BrandNamesRequest(categoryCodes);
    String requestBody = this.objectMapper.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void getDefaultBrandsTest() throws Exception {
    Mockito.when(this.brandService.getDefaultBrands(BrandControllerTest.DEFAULT_STORE_ID))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValueResponse()));
    URI uri = new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.GET_DEFAULT_BRANDS)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService).getDefaultBrands(BrandControllerTest.DEFAULT_STORE_ID);
  }

  @Test
  public void getDefaultBrandsExceptionTest() throws Exception {
    Mockito.when(this.brandService.getDefaultBrands(BrandControllerTest.DEFAULT_STORE_ID)).thenThrow(Exception.class);
    URI uri = new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.GET_DEFAULT_BRANDS)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.brandService).getDefaultBrands(BrandControllerTest.DEFAULT_STORE_ID);
  }

  @Test
  public void getValidBrandSummary() throws Exception {
    Mockito.when(this.brandService.getBrandSummaryResponseForValidBrands(BrandControllerTest.DEFAULT_STORE_ID, 0, 10))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    URI uri = new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.GET_VALID_BRAND_SUMMARY)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).addParameter("page", "0")
        .addParameter("size", "10").build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService)
        .getBrandSummaryResponseForValidBrands(BrandControllerTest.DEFAULT_STORE_ID, 0, 10);
  }

  @Test
  public void getValidBrandSummaryExceptionTest() throws Exception {
    Mockito.when(this.brandService.getBrandSummaryResponseForValidBrands(BrandControllerTest.DEFAULT_STORE_ID, 0, 10))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        String.format(ErrorMessage.GREATER_THAN_MAX_ALLOWED_SIZE_ERROR.getMessage(), 10, 9)));
    URI uri = new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.GET_VALID_BRAND_SUMMARY)
        .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).addParameter("page", "0")
        .addParameter("size", "10").build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.brandService)
        .getBrandSummaryResponseForValidBrands(BrandControllerTest.DEFAULT_STORE_ID, 0, 10);
  }

  @Test
  public void getProtectedBrandList() throws Exception {
    ProtectedBrandResponse protectedBrandResponse = new ProtectedBrandResponse();
    protectedBrandResponse.setBrandName(DEFAULT_BRAND_NAME);
    protectedBrandResponse.setBrandCode(DEFAULT_BRAND_CODE);
    List<ProtectedBrandResponse> brandList = new ArrayList<>();
    brandList.add(protectedBrandResponse);
    Mockito.when(this.brandService.getProtectedBrandList(BrandControllerTest.DEFAULT_STORE_ID))
      .thenReturn(brandList);
    URI uri = new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.PROTECTED_BRANDS)
      .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
      .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
      .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
      .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandService)
      .getProtectedBrandList(BrandControllerTest.DEFAULT_STORE_ID);
  }

  @Test
  public void getProtectedBrandListExceptionTest() throws Exception {
    Mockito.when(this.brandService.getProtectedBrandList(BrandControllerTest.DEFAULT_STORE_ID))
      .thenThrow(new ApplicationRuntimeException());
    URI uri = new URIBuilder().setPath(BrandControllerPath.BASE_PATH + BrandControllerPath.PROTECTED_BRANDS)
      .addParameter("storeId", BrandControllerTest.DEFAULT_STORE_ID)
      .addParameter("channelId", BrandControllerTest.DEFAULT_CHANNEL_ID)
      .addParameter("clientId", BrandControllerTest.DEFAULT_CLIENT_ID)
      .addParameter("requestId", BrandControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("username", BrandControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.brandService)
      .getProtectedBrandList(BrandControllerTest.DEFAULT_STORE_ID);
  }
}
