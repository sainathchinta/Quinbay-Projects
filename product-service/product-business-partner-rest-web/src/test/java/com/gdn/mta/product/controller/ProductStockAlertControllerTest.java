package com.gdn.mta.product.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.gdn.mta.product.service.ProductStockAlertWrapper;

import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
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

import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.service.ProductStockAlertService;
import com.gdn.mta.product.web.model.ProductStockAlertControllerPath;
import com.gdn.x.businesspartner.entity.Profile;
import com.gdn.x.businesspartner.entity.ResponsiblePerson;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;

public class ProductStockAlertControllerTest {

	private static final String DEFAULT_REQUEST_ID = UUID.randomUUID()
			.toString();
	private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00001";
	private static final String DEFAULT_STORE_ID = "10001";
	private static final String DEFAULT_CHANNEL_ID = "api";
	private static final String DEFAULT_USERNAME = "com.gdn.mta";
	private static final String DEFAULT_CLIENT_ID = "mta";
	private static final String DEFAULT_EMAIL = "mta@mta.com";
	private static final String DEFAULT_MAX_ATTEMPT = "3";
	private static final int BATCH_SIZE = 100;

	@InjectMocks
	private ProductStockAlertController productStockAlertController;

	@Mock
	private ProductLevel3Service productLevel3Service;

	@Mock
	private ProductStockAlertService productStockAlertService;

	@Mock
	private ProductStockAlertWrapper productStockAlertWrapper;

	@Mock
	private ApplicationProperties applicationProperties;

	private MockMvc mockMvc;
	private List<PbpStockAlert> listPbpStockAlert;
	private List<String> listProductStockAlert;
	private PbpStockAlert pbpStockAlert;

	public ProductStockAlertController getProductStockAlertController() {
		return productStockAlertController;
	}

	public void setProductStockAlertController(
			ProductStockAlertController productStockAlertController) {
		this.productStockAlertController = productStockAlertController;
	}

	public ProductStockAlertService getProductStockAlertService() {
		return productStockAlertService;
	}

	public void setProductStockAlertService(
			ProductStockAlertService productStockAlertService) {
		this.productStockAlertService = productStockAlertService;
	}

	public ApplicationProperties getApplicationProperties() {
		return applicationProperties;
	}

	public void setApplicationProperties(
			ApplicationProperties applicationProperties) {
		this.applicationProperties = applicationProperties;
	}

	public ProductLevel3Service getProductLevel3Service() {
		return productLevel3Service;
	}

	public void setProductLevel3Service(
			ProductLevel3Service productLevel3Service) {
		this.productLevel3Service = productLevel3Service;
	}

	public MockMvc getMockMvc() {
		return mockMvc;
	}

	public void setMockMvc(MockMvc mockMvc) {
		this.mockMvc = mockMvc;
	}

	private List<PbpStockAlert> createListPbpStockAlert() {
		List<PbpStockAlert> listPbpStockAlert = new ArrayList<PbpStockAlert>();
		for (int j = 1; j <= 25; j++) {
			pbpStockAlert.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
			listPbpStockAlert.add(pbpStockAlert);
		}
		return listPbpStockAlert;
	}

	@BeforeEach
	public void initializeTest() throws Exception {
		MockitoAnnotations.initMocks(this);
		setMockMvc(MockMvcBuilders
				.standaloneSetup(getProductStockAlertController())
				.setMessageConverters(new ByteArrayHttpMessageConverter(),
						new StringHttpMessageConverter(),
						new ResourceHttpMessageConverter(),
						new FormHttpMessageConverter(),
						new MappingJackson2HttpMessageConverter()).build());
		pbpStockAlert = new PbpStockAlert();
		pbpStockAlert.setId(DEFAULT_REQUEST_ID);
		listPbpStockAlert = new ArrayList<PbpStockAlert>();
		listPbpStockAlert.add(pbpStockAlert);
		listPbpStockAlert.get(0).setBusinessPartnerCode(
				DEFAULT_BUSINESS_PARTNER_CODE);
		listProductStockAlert = new ArrayList<String>();
		listProductStockAlert.add(DEFAULT_BUSINESS_PARTNER_CODE);
		Profile businessPartner = new Profile();
		businessPartner.setResponsiblePerson(new ResponsiblePerson());
		businessPartner.getResponsiblePerson().setEmail(DEFAULT_EMAIL);
		Mockito.when(applicationProperties.getMaxStockAlertAttempt())
				.thenReturn(DEFAULT_MAX_ATTEMPT);
		Mockito.when(
				getProductStockAlertService()
						.findGdnSkuStockAlertByBusinessPartnerCode(
								Mockito.anyString())).thenReturn(
				listProductStockAlert);
		Mockito.doNothing()
				.when(getProductLevel3Service())
				.updateItemViewConfig(Mockito.any(ItemViewConfigRequest.class),
						Mockito.anyString(), Mockito.anyString());
	}

	@Test
	public void updateItemViewConfigTest() throws Exception {
		URI uri = new URIBuilder()
				.setPath(
						ProductStockAlertControllerPath.BASE_PATH
								+ ProductStockAlertControllerPath.UPDATE_ITEM_STOCK_ALERT_VIEW_CONFIG)
				.addParameter("storeId",
						ProductStockAlertControllerTest.DEFAULT_STORE_ID)
				.addParameter("channelId",
						ProductStockAlertControllerTest.DEFAULT_CHANNEL_ID)
				.addParameter("requestId",
						ProductStockAlertControllerTest.DEFAULT_REQUEST_ID)
				.addParameter("username",
						ProductStockAlertControllerTest.DEFAULT_USERNAME)
				.addParameter("clientId",
						ProductStockAlertControllerTest.DEFAULT_CLIENT_ID)
				.build();

		this.mockMvc.perform(
				MockMvcRequestBuilders.get(uri)
						.accept(MediaType.APPLICATION_JSON)
						.contentType(MediaType.APPLICATION_JSON)).andExpect(
				MockMvcResultMatchers.status().isOk());

		Mockito.verify(getProductStockAlertService())
				.autoBatchUpdateItemViewConfig();
	}

	@Test
	public void sendMailItemStockAlertTest() throws Exception {
		URI uri = new URIBuilder()
				.setPath(
						ProductStockAlertControllerPath.BASE_PATH
								+ ProductStockAlertControllerPath.EMAIL_ITEM_STOCK_ALERT)
				.addParameter("storeId",
						ProductStockAlertControllerTest.DEFAULT_STORE_ID)
				.addParameter("channelId",
						ProductStockAlertControllerTest.DEFAULT_CHANNEL_ID)
				.addParameter("requestId",
						ProductStockAlertControllerTest.DEFAULT_REQUEST_ID)
				.addParameter("username",
						ProductStockAlertControllerTest.DEFAULT_USERNAME)
				.addParameter("clientId",
						ProductStockAlertControllerTest.DEFAULT_CLIENT_ID)
				.build();

		this.mockMvc.perform(
				MockMvcRequestBuilders.get(uri)
						.accept(MediaType.APPLICATION_JSON)
						.contentType(MediaType.APPLICATION_JSON)).andExpect(
				MockMvcResultMatchers.status().isOk());

		Mockito.verify(productStockAlertWrapper)
				.sendMailAndNotification(DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
	}

	@Test
	public void findSkuStockAlertByBusinessPartnerCodeTest() throws Exception {
		URI uri = new URIBuilder()
				.setPath(
						ProductStockAlertControllerPath.BASE_PATH
								+ ProductStockAlertControllerPath.FIND_SKU_STOCK_ALERT_BY_BUSINESS_PARTNER_CODE)
				.addParameter("storeId",
						ProductStockAlertControllerTest.DEFAULT_STORE_ID)
				.addParameter("channelId",
						ProductStockAlertControllerTest.DEFAULT_CHANNEL_ID)
				.addParameter("requestId",
						ProductStockAlertControllerTest.DEFAULT_REQUEST_ID)
				.addParameter("username",
						ProductStockAlertControllerTest.DEFAULT_USERNAME)
				.addParameter("clientId",
						ProductStockAlertControllerTest.DEFAULT_CLIENT_ID)
				.addParameter(
						"businessPartnerCode",
						ProductStockAlertControllerTest.DEFAULT_BUSINESS_PARTNER_CODE)
				.build();

		this.mockMvc.perform(
				MockMvcRequestBuilders.get(uri)
						.accept(MediaType.APPLICATION_JSON)
						.contentType(MediaType.APPLICATION_JSON)).andExpect(
				MockMvcResultMatchers.status().isOk());

		Mockito.verify(getProductStockAlertService())
				.findGdnSkuStockAlertByBusinessPartnerCode(Mockito.anyString());
	}

	@Test
	public void hideItemSkuByOOSDateTest() throws Exception {
    Mockito.doNothing().when(this.productStockAlertService)
        .hideItemSkuByOOSDate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, BATCH_SIZE);
		URI uri = new URIBuilder()
				.setPath(ProductStockAlertControllerPath.BASE_PATH + ProductStockAlertControllerPath.HIDE_OOS_ITEMS)
				.addParameter("storeId", ProductStockAlertControllerTest.DEFAULT_STORE_ID)
				.addParameter("channelId", ProductStockAlertControllerTest.DEFAULT_CHANNEL_ID)
				.addParameter("requestId", ProductStockAlertControllerTest.DEFAULT_REQUEST_ID)
				.addParameter("username", ProductStockAlertControllerTest.DEFAULT_USERNAME)
				.addParameter("clientId", ProductStockAlertControllerTest.DEFAULT_CLIENT_ID)
				.addParameter("batchSize", String.valueOf(ProductStockAlertControllerTest.BATCH_SIZE)).build();
		this.mockMvc.perform(
				MockMvcRequestBuilders.get(uri)
						.accept(MediaType.APPLICATION_JSON)
						.contentType(MediaType.APPLICATION_JSON)).andExpect(
				MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
		Mockito.verify(this.productStockAlertService).hideItemSkuByOOSDate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, BATCH_SIZE);
	}

	@Test
	public void hideItemSkuByOOSDateTest_throwsException() throws Exception {
		Mockito.doThrow(RuntimeException.class).when(this.productStockAlertService)
				.hideItemSkuByOOSDate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, BATCH_SIZE);
		URI uri = new URIBuilder()
				.setPath(ProductStockAlertControllerPath.BASE_PATH + ProductStockAlertControllerPath.HIDE_OOS_ITEMS)
				.addParameter("storeId", ProductStockAlertControllerTest.DEFAULT_STORE_ID)
				.addParameter("channelId", ProductStockAlertControllerTest.DEFAULT_CHANNEL_ID)
				.addParameter("requestId", ProductStockAlertControllerTest.DEFAULT_REQUEST_ID)
				.addParameter("username", ProductStockAlertControllerTest.DEFAULT_USERNAME)
				.addParameter("clientId", ProductStockAlertControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("batchSize", String.valueOf(ProductStockAlertControllerTest.BATCH_SIZE)).build();
		try {
      this.mockMvc.perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
		  throw new Exception(e);
    } finally {
      Mockito.verify(this.productStockAlertService).hideItemSkuByOOSDate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, BATCH_SIZE);
    }
	}

	@Test
	public void findSkuStockAlertByBusinessPartnerCodeExceptionTest()
			throws Exception {
		Mockito.doThrow(RuntimeException.class).when(getProductStockAlertService())
				.findGdnSkuStockAlertByBusinessPartnerCode(Mockito.anyString());

		URI uri = new URIBuilder()
				.setPath(
						ProductStockAlertControllerPath.BASE_PATH
								+ ProductStockAlertControllerPath.FIND_SKU_STOCK_ALERT_BY_BUSINESS_PARTNER_CODE)
				.addParameter("storeId",
						ProductStockAlertControllerTest.DEFAULT_STORE_ID)
				.addParameter("channelId",
						ProductStockAlertControllerTest.DEFAULT_CHANNEL_ID)
				.addParameter("requestId",
						ProductStockAlertControllerTest.DEFAULT_REQUEST_ID)
				.addParameter("username",
						ProductStockAlertControllerTest.DEFAULT_USERNAME)
				.addParameter("clientId",
						ProductStockAlertControllerTest.DEFAULT_CLIENT_ID)
				.addParameter(
						"businessPartnerCode",
						ProductStockAlertControllerTest.DEFAULT_BUSINESS_PARTNER_CODE)
				.build();

		try {
			this.mockMvc.perform(
					MockMvcRequestBuilders.get(uri)
							.accept(MediaType.APPLICATION_JSON)
							.contentType(MediaType.APPLICATION_JSON))
					.andExpect(MockMvcResultMatchers.status().isOk());
		} catch (Exception e) {
			Mockito.verify(getProductStockAlertService())
					.findGdnSkuStockAlertByBusinessPartnerCode(
							Mockito.anyString());
		}
	}

	@AfterEach
	public void finalizeTest() throws Exception {
		Mockito.verifyNoMoreInteractions(getProductLevel3Service());
		Mockito.verifyNoMoreInteractions(getProductStockAlertService());
		Mockito.verifyNoMoreInteractions(applicationProperties);
	}

}
