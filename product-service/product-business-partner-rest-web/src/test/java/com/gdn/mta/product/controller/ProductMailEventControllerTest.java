package com.gdn.mta.product.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.UUID;

import com.gdn.common.exception.ApplicationRuntimeException;
import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.mta.product.service.ProductMailEventService;

public class ProductMailEventControllerTest {

  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "api";
  private static final String DEFAULT_USERNAME = "com.gdn.mta";
  private static final String DEFAULT_CLIENT_ID = "mta";

  private ArgumentCaptor<Date> dateArgumentCaptor = ArgumentCaptor.forClass(Date.class);

  @Mock
  private ProductMailEventService productMailEventService;

  @InjectMocks
  private ProductMailEventController productMailEventController;

  private MockMvc mockMvc;

  @BeforeEach
  public void setUp(){
    MockitoAnnotations.initMocks(this);
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.productMailEventController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(),
            new StringHttpMessageConverter(), new ResourceHttpMessageConverter(),
            new FormHttpMessageConverter(), new MappingJackson2HttpMessageConverter()).build();

  }

  @Test
  public void sendMail() throws Exception {
    doNothing().when(this.productMailEventService).sendProductMailEventsToBusinessPartners(any());
    URI uri = new URIBuilder().setPath(ProductMailEventController.BASE_PATH + "/" +
        ProductMailEventController.SEND_MAIL).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.productMailEventService).sendProductMailEventsToBusinessPartners(dateArgumentCaptor
        .capture());
    Date date = dateArgumentCaptor.getValue();
    Integer day = Integer.parseInt(new SimpleDateFormat("dd").format(date));
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE , -1);
    Assertions.assertEquals(day.intValue(), calendar.get(Calendar.DAY_OF_MONTH));
  }


  @Test
  public void sendMail_Fail() throws Exception {
    doThrow(Exception.class).when(this.productMailEventService)
        .sendProductMailEventsToBusinessPartners(any());
    URI uri = new URIBuilder().setPath(ProductMailEventController.BASE_PATH + "/" +
        ProductMailEventController.SEND_MAIL).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri))
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.productMailEventService).sendProductMailEventsToBusinessPartners(dateArgumentCaptor
        .capture());
    Date date = dateArgumentCaptor.getValue();
    Integer day = Integer.parseInt(new SimpleDateFormat("dd").format(date));
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE , -1);
    Assertions.assertEquals(day.intValue(), calendar.get(Calendar.DAY_OF_MONTH));
  }

  @Test
  public void sendPostLiveReviewActivatedMailTest() throws Exception {
    URI uri = new URIBuilder().setPath(ProductMailEventController.BASE_PATH + "/" +
        ProductMailEventController.SEND_POST_LIVE_REVIEW_ACTIVATED_MAIL)
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.productMailEventService).sendPostLiveReviewActiveProductMailEventsToBusinessPartners(
        dateArgumentCaptor.capture());
    Integer day = Integer.parseInt(new SimpleDateFormat("dd").format(dateArgumentCaptor.getValue()));
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE , -1);
    Assertions.assertEquals(day.intValue(), calendar.get(Calendar.DAY_OF_MONTH));
  }

  @Test
  public void sendPostLiveReviewActivatedMailErrorTest() throws Exception {
    doThrow(RuntimeException.class).when(this.productMailEventService)
        .sendPostLiveReviewActiveProductMailEventsToBusinessPartners(any());
    URI uri = new URIBuilder().setPath(ProductMailEventController.BASE_PATH + "/" +
        ProductMailEventController.SEND_POST_LIVE_REVIEW_ACTIVATED_MAIL)
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri))
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.productMailEventService).sendPostLiveReviewActiveProductMailEventsToBusinessPartners(
        dateArgumentCaptor.capture());
    Integer day = Integer.parseInt(new SimpleDateFormat("dd").format(dateArgumentCaptor.getValue()));
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE , -1);
    Assertions.assertEquals(day.intValue(), calendar.get(Calendar.DAY_OF_MONTH));
  }

  @Test
  public void sendPostLiveReviewRejectedMailTest() throws Exception {
    URI uri = new URIBuilder().setPath(ProductMailEventController.BASE_PATH + "/" +
        ProductMailEventController.SEND_POST_LIVE_REVIEW_REJECTED_MAIL)
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.productMailEventService).sendPostLiveReviewRejectProductMailEventsToBusinessPartners(
        dateArgumentCaptor.capture());
    Integer day = Integer.parseInt(new SimpleDateFormat("dd").format(dateArgumentCaptor.getValue()));
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE , -1);
    Assertions.assertEquals(day.intValue(), calendar.get(Calendar.DAY_OF_MONTH));
  }

  @Test
  public void sendPostLiveReviewRejectedMailErrorTest() throws Exception {
    doThrow(RuntimeException.class).when(this.productMailEventService)
        .sendPostLiveReviewRejectProductMailEventsToBusinessPartners(any());
    URI uri = new URIBuilder().setPath(ProductMailEventController.BASE_PATH + "/" +
        ProductMailEventController.SEND_POST_LIVE_REVIEW_REJECTED_MAIL)
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri))
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.productMailEventService).sendPostLiveReviewRejectProductMailEventsToBusinessPartners(
        dateArgumentCaptor.capture());
    Integer day = Integer.parseInt(new SimpleDateFormat("dd").format(dateArgumentCaptor.getValue()));
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE , -1);
    Assertions.assertEquals(day.intValue(), calendar.get(Calendar.DAY_OF_MONTH));
  }

  @Test
  public void sendMailSuspension() throws Exception {
    doNothing().when(this.productMailEventService).sendProductMailEventsToBusinessPartnersForSuspension(any());
    URI uri = new URIBuilder()
        .setPath(ProductMailEventController.BASE_PATH + "/" + ProductMailEventController.SEND_MAIL_SUSPENSION)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.productMailEventService)
        .sendProductMailEventsToBusinessPartnersForSuspension(dateArgumentCaptor.capture());
    Date date = dateArgumentCaptor.getValue();
    Integer day = Integer.parseInt(new SimpleDateFormat("dd").format(date));
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -1);
    Assertions.assertEquals(day.intValue(), calendar.get(Calendar.DAY_OF_MONTH));
  }

  @Test
  public void sendMailSuspension_Fail() throws Exception {
    doThrow(Exception.class).when(this.productMailEventService)
        .sendProductMailEventsToBusinessPartnersForSuspension(any());
    URI uri = new URIBuilder()
        .setPath(ProductMailEventController.BASE_PATH + "/" + ProductMailEventController.SEND_MAIL_SUSPENSION)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri)).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.productMailEventService)
        .sendProductMailEventsToBusinessPartnersForSuspension(dateArgumentCaptor.capture());
    Date date = dateArgumentCaptor.getValue();
    Integer day = Integer.parseInt(new SimpleDateFormat("dd").format(date));
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -1);
    Assertions.assertEquals(day.intValue(), calendar.get(Calendar.DAY_OF_MONTH));
  }

  @Test
  public void deleteOldRecordsByDays() throws Exception {
    doNothing().when(this.productMailEventService).deleteOldRecordsByDays(5);
    URI uri = new URIBuilder().setPath(ProductMailEventController.BASE_PATH + "/"
        + ProductMailEventController.DELETE_OLD_RECORDS_BY_DAYS.replaceAll("\\{days\\}", "5"))
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("username", DEFAULT_USERNAME).build();
    this.mockMvc.perform(delete(uri)).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.productMailEventService).deleteOldRecordsByDays(5);
  }

  @Test
  public void deleteOldRecordsByDaysException() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(this.productMailEventService)
      .deleteOldRecordsByDays(5);
    URI uri = new URIBuilder().setPath(ProductMailEventController.BASE_PATH + "/"
        + ProductMailEventController.DELETE_OLD_RECORDS_BY_DAYS.replaceAll("\\{days\\}", "5"))
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("username", DEFAULT_USERNAME).build();
    this.mockMvc.perform(delete(uri)).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.productMailEventService).deleteOldRecordsByDays(5);
  }

}