package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.UUID;

import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.web.servlet.MockMvc;

import com.gdn.x.productcategorybase.service.MailService;

public class MailEventControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_USERNAME = "com.gdn.mta";
  private static final String DEFAULT_CLIENT_ID = "CLIENT";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();

  private MockMvc mockMvc;

  @Mock
  private MailService mailService;

  @InjectMocks
  private MailEventController mailEventController;

  private ArgumentCaptor<Date> dateArgumentCaptor = ArgumentCaptor.forClass(Date.class);

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.mailEventController).build();
  }

  @Test
  public void sendConfigurationMailForCategoryTest() throws Exception {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -1);
    Date date = calendar.getTime();
    Mockito.doNothing().when(this.mailService).sendConfigurationChangesMailForCategory(date);
    URI uri =
        new URIBuilder().setPath(MailEventController.BASE_PATH + MailEventController.SEND_CATEGORY_CONFIG_MAIL)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.mailService).sendConfigurationChangesMailForCategory(dateArgumentCaptor.capture());
    Date date2 = dateArgumentCaptor.getValue();
    Integer day = Integer.parseInt(new SimpleDateFormat("dd").format(date2));
    Calendar calendar2 = Calendar.getInstance();
    calendar2.add(Calendar.DATE, -1);
    Assertions.assertEquals(day.intValue(), calendar2.get(Calendar.DAY_OF_MONTH));
  }

  @Test
  public void sendConfigurationMailForMerchantTest() throws Exception {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -1);
    Date date = calendar.getTime();
    Mockito.doNothing().when(this.mailService).sendConfigurationChangesMailForMerchant(date);
    URI uri =
        new URIBuilder().setPath(MailEventController.BASE_PATH + MailEventController.SEND_MERCHANT_CONFIG_MAIL)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.mailService).sendConfigurationChangesMailForMerchant(dateArgumentCaptor.capture());
    Date date2 = dateArgumentCaptor.getValue();
    Integer day = Integer.parseInt(new SimpleDateFormat("dd").format(date2));
    Calendar calendar2 = Calendar.getInstance();
    calendar2.add(Calendar.DATE, -1);
    Assertions.assertEquals(day.intValue(), calendar2.get(Calendar.DAY_OF_MONTH));
  }

  @Test
  public void sendConfigurationMailForCategoryTest_Fail() throws Exception {
    doThrow(Exception.class).when(this.mailService).sendConfigurationChangesMailForCategory(any());
    URI uri =
        new URIBuilder().setPath(MailEventController.BASE_PATH + MailEventController.SEND_CATEGORY_CONFIG_MAIL)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri)).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.mailService).sendConfigurationChangesMailForCategory(dateArgumentCaptor.capture());
    Date date = dateArgumentCaptor.getValue();
    Integer day = Integer.parseInt(new SimpleDateFormat("dd").format(date));
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -1);
    Assertions.assertEquals(day.intValue(), calendar.get(Calendar.DAY_OF_MONTH));
  }

  @Test
  public void sendConfigurationMailForMerchantTest_Fail() throws Exception {
    doThrow(Exception.class).when(this.mailService).sendConfigurationChangesMailForMerchant(any());
    URI uri =
        new URIBuilder().setPath(MailEventController.BASE_PATH + MailEventController.SEND_MERCHANT_CONFIG_MAIL)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    this.mockMvc.perform(get(uri)).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.mailService).sendConfigurationChangesMailForMerchant(dateArgumentCaptor.capture());
    Date date = dateArgumentCaptor.getValue();
    Integer day = Integer.parseInt(new SimpleDateFormat("dd").format(date));
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -1);
    Assertions.assertEquals(day.intValue(), calendar.get(Calendar.DAY_OF_MONTH));
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(mailService);
  }
}
