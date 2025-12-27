package com.gdn.mta.bulk.service.download;

import static org.mockito.MockitoAnnotations.initMocks;

import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.mta.bulk.helper.BulkOrderProcessHelper;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.service.FileStorageService;

public class BulkProcessFileGenerationTest {

    private static final String DEFAULT_EXCEPTION_MESSAGE = "EXCEPTION MESSAGE";
    private static final String DEFAULT_REQUEST_ID = "REQUEST ID";
    private static final String DEFAULT_FILE_PATH = "FILE_PATH";
    private static final String DEFAULT_DIRECTORY = "DIRECTORY";

    private BulkDownloadRequest bulkDownloadRequestCSV;
    private BulkDownloadRequest bulkDownloadRequestXSLSM;
    private BulkDownloadRequest bulkDownloadRequestXLSX;
    private BulkDownloadRequest bulkDownloadRequestXLS;
    private BulkDataResponse bulkDataResponse;
    private Workbook workbook;

    @Mock
    private BulkOrderProcessHelper bulkProcessHelper;

    @Mock
    private FileStorageService fileStorageService;

    @InjectMocks
    private BulkProcessFileGeneration bulkProcessFileGeneration;

    @BeforeEach
    public void initialize() throws Exception {
        initMocks(this);
        bulkDownloadRequestCSV = new BulkDownloadRequest.BulkRequestBuilder()
            .fileType(FileType.CSV).build();
        bulkDownloadRequestCSV.setDirectDownload(true);
        bulkDownloadRequestCSV.setExceptionMsg(DEFAULT_EXCEPTION_MESSAGE);
        bulkDownloadRequestCSV.setRequestId(DEFAULT_REQUEST_ID);

        bulkDownloadRequestXLSX = new BulkDownloadRequest.BulkRequestBuilder()
            .fileType(FileType.XLSX).build();
        bulkDownloadRequestXLSX.setDirectDownload(true);
        bulkDownloadRequestXLSX.setExceptionMsg(DEFAULT_EXCEPTION_MESSAGE);
        bulkDownloadRequestXLSX.setRequestId(DEFAULT_REQUEST_ID);

        bulkDownloadRequestXLS = new BulkDownloadRequest.BulkRequestBuilder()
            .fileType(FileType.XLS).build();
        bulkDownloadRequestXLS.setDirectDownload(true);
        bulkDownloadRequestXLS.setExceptionMsg(DEFAULT_EXCEPTION_MESSAGE);
        bulkDownloadRequestXLS.setRequestId(DEFAULT_REQUEST_ID);

        bulkDownloadRequestXSLSM = new BulkDownloadRequest.BulkRequestBuilder()
            .fileType(FileType.XLSM).build();
        bulkDownloadRequestXSLSM.setDirectDownload(true);
        bulkDownloadRequestXSLSM.setExceptionMsg(DEFAULT_EXCEPTION_MESSAGE);
        bulkDownloadRequestXSLSM.setRequestId(DEFAULT_REQUEST_ID);

        bulkDataResponse = new BulkDataResponse();

        workbook = new SXSSFWorkbook();

        Mockito.when(bulkProcessHelper.getDirectory(Mockito.any(BulkDownloadRequest.class))).thenReturn(DEFAULT_DIRECTORY);
    }

    @Test
    public void generateFileFromResponseCSVTest() throws Exception{
        this.bulkProcessFileGeneration.generateFileFromResponse(bulkDownloadRequestCSV, bulkDataResponse, bulkProcessHelper);
    }

    @Test
    public void generateFileFromResponseXLSMTest() throws Exception{
        this.bulkProcessFileGeneration.generateFileFromResponse(bulkDownloadRequestXSLSM, bulkDataResponse, bulkProcessHelper);
    }

    @Test
    public void generateFileFromResponseCSVWithExceptionTest() throws Exception{
        Mockito.doThrow(Exception.class).when(fileStorageService).generateFile(Mockito.any(BulkDownloadRequest.class), Mockito.any());
        Assertions.assertThrows(Exception.class,
            () -> this.bulkProcessFileGeneration.generateFileFromResponse(bulkDownloadRequestCSV,
                bulkDataResponse, bulkProcessHelper));
    }

    @Test
    @SuppressWarnings("unchecked")
    public void generateFileFromResponseXLSXTest() throws Exception{
        Mockito.when(bulkProcessHelper.generateDataSheet(Mockito.anyList(), Mockito.anyList(), Mockito.eq(0)))
            .thenReturn(workbook);
        Mockito.when(bulkProcessHelper.modifyWorkbook(Mockito.any(Workbook.class), Mockito.any(BulkDataResponse.class))).thenReturn(workbook);
        Mockito.when(bulkProcessHelper.getFilePath(Mockito.anyString(), Mockito.anyString())).thenReturn(DEFAULT_FILE_PATH);
        this.bulkProcessFileGeneration.generateFileFromResponse(bulkDownloadRequestXLSX, bulkDataResponse, bulkProcessHelper);
    }

    @Test
    public void generateFileResponseXLSXTest() throws Exception{
        Mockito.when(bulkProcessHelper.generateDataSheet(Mockito.anyList(), Mockito.anyList(), Mockito.eq(0)))
            .thenReturn(workbook);
        Mockito.when(bulkProcessHelper.modifyWorkbook(Mockito.any(Workbook.class), Mockito.any(BulkDataResponse.class))).thenReturn(workbook);
        Mockito.when(bulkProcessHelper.getFilePath(Mockito.anyString(), Mockito.anyString())).thenReturn(DEFAULT_FILE_PATH);
        this.bulkProcessFileGeneration.generateFileResponse(bulkDownloadRequestXLSX, bulkDataResponse, bulkProcessHelper);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void generateFileFromResponseXLSTest() throws Exception{
        this.bulkProcessFileGeneration.generateFileFromResponse(bulkDownloadRequestXLS, bulkDataResponse, bulkProcessHelper);
    }

}
