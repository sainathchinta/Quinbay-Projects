package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.DownloadApiPath;
import com.gdn.partners.pcu.external.properties.SystemParameterProperties;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.io.File;
import java.nio.file.Path;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class DownloadControllerTest extends TestHelper {

    private static final String DATE = "DATE";
    private static final String UPDATE_REASON = "UPDATE_REASON";
    private static final String DEFAULT_FILE_NAME = "null_" + UPDATE_REASON + "_" + DATE + ".xlsx";
    private static final String EXCEL_FOLDER = "EXCEL_FOLDER";

    @TempDir
    Path tempDir;

    @Mock
    private MandatoryParameterHelper mandatoryParameterHelper;

    @Mock
    private SystemParameterProperties systemParameterProperties;

    @InjectMocks
    private DownloadController downloadController;

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.openMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(downloadController).build();
    }

    @AfterEach
    public void tearDown() {
        verifyNoMoreInteractions(mandatoryParameterHelper);
        verifyNoMoreInteractions(systemParameterProperties);
    }

    @Test
    public void downloadFromPathTest() throws Exception {
        when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
                Constants.BUSINESS_PARTNER_CODE);
        when(this.systemParameterProperties.getDirectoryStockAlertsExcel()).thenReturn(EXCEL_FOLDER);
        
        File folder = tempDir.resolve(UPDATE_REASON).resolve("null").resolve(DATE).toFile();
        folder.mkdirs();
        File myFile = new File(folder, DEFAULT_FILE_NAME);

        MockHttpServletRequestBuilder requestBuilder =
                get(DownloadApiPath.BASE_PATH + DownloadApiPath.DOWNLOAD_FILE, DATE,
                        UPDATE_REASON).contentType(MediaType.APPLICATION_OCTET_STREAM)
                        .sessionAttr(Constants.SESSION, getDefaultSession());

        mockMvc.perform(requestBuilder).andExpect(status().isOk());

        verify(this.mandatoryParameterHelper).getBusinessPartnerCode();
        verify(this.systemParameterProperties).getDirectoryStockAlertsExcel();
    }

    @Test
    public void downloadFromPathTest_expectExceptionTest() throws Exception {
        when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
                Constants.BUSINESS_PARTNER_CODE);
        when(this.systemParameterProperties.getDirectoryStockAlertsExcel()).thenThrow(
                RuntimeException.class);
        
        File folder = tempDir.resolve(UPDATE_REASON).resolve("null").resolve(DATE).toFile();
        folder.mkdirs();
        File myFile = new File(folder, DEFAULT_FILE_NAME);

        try {
            MockHttpServletRequestBuilder requestBuilder =
                    get(DownloadApiPath.BASE_PATH + DownloadApiPath.DOWNLOAD_FILE, DATE,
                            UPDATE_REASON).contentType(MediaType.APPLICATION_OCTET_STREAM)
                            .sessionAttr(Constants.SESSION, getDefaultSession());

            mockMvc.perform(requestBuilder).andExpect(status().isOk());
        } catch (Exception e) {
        } finally {
            verify(this.mandatoryParameterHelper).getBusinessPartnerCode();
            verify(this.systemParameterProperties).getDirectoryStockAlertsExcel();
        }
    }
}
