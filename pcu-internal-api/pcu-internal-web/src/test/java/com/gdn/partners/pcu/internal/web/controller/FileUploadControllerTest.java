package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.FileUploadApiPath;
import com.gdn.partners.pcu.internal.service.FileHelper;
import com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import org.apache.commons.io.FileUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.io.File;
import java.io.IOException;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class FileUploadControllerTest extends TestHelper {

  private static final String FILE = "/filestore/originalFilename.xls";
  private static final String PATH = "path";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private byte[] fileContent;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private FileHelper fileHelper;

  @InjectMocks
  private FileUploadController fileUploadController;

  @BeforeEach
  public void init() throws IOException {
    mockMvc = MockMvcBuilders.standaloneSetup(fileUploadController).build();
    fileContent = new byte[] {-1, -40, -20, -10};
  }

  @AfterEach
  public void tearDown() throws IOException {
    Mockito.verifyNoMoreInteractions(fileHelper);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  void bulkUploadFileAssigneeTest() throws Exception {
    mockFile(PATH + FILE);
    MockMultipartFile file =
        new MockMultipartFile("file", ORIGINAL_FILENAME, "application/vnd" + ".ms-excel",
            FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);

    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
                FileUploadApiPath.BASE_PATH + FileUploadApiPath.BULK_UPLOAD).file(file)
            .accept(MediaType.APPLICATION_JSON)
            .param("processType", BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));

    Mockito.verify(fileHelper)
        .uploadBulkFile(file, BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name(),
            Constants.REQUEST_ID, Constants.STORE_ID, Constants.USER_NAME);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }
}