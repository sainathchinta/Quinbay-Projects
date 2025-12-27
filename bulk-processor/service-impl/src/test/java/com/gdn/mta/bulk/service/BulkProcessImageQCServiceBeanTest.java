package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessImageQC;
import com.gdn.mta.bulk.repository.BulkProcessImageQCRepository;
import com.gdn.partners.bulk.util.Constant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

public class BulkProcessImageQCServiceBeanTest {

    private static final String STORE_ID = "10001";

    @InjectMocks
    BulkProcessImageQCServiceBean bulkProcessImageQCServiceBean;

    @Mock
    BulkProcessImageQCRepository bulkProcessImageQCRepository;

    private static final String BULK_PROCESS_CODE = "bulkCode";
    private static final String PARENT_PRODUCT = "parentProduct";
    private static final String URL = "url";
    private static final String URL_1 = "url1";

    private List<String> imageUrl;
    private BulkProcessImageQC bulkProcessImageQC;
    private BulkProcessImageQC bulkProcessImageQC1;
    List<BulkProcessImageQC> bulkProcessImageQCList = new ArrayList<>();

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        imageUrl = new ArrayList<>();
        imageUrl.add(URL);
        imageUrl.add(URL_1);

        bulkProcessImageQC = new BulkProcessImageQC();
        bulkProcessImageQC.setCompleted(true);
        bulkProcessImageQC1 = new BulkProcessImageQC();
        bulkProcessImageQC1.setCompleted(false);
        bulkProcessImageQCList.add(bulkProcessImageQC);
        bulkProcessImageQCList.add(bulkProcessImageQC1);
    }

    @AfterEach
    public void tearDown() throws Exception {
        Mockito.verifyNoMoreInteractions(bulkProcessImageQCRepository);
    }

    @Test
    public void findByStoreIdAndBulkProcessCodeTest() {
        bulkProcessImageQCServiceBean.fetchAllBulkProcessImageQCByBulkProcess(Constant.STORE_ID,
            new BulkProcess());
        Mockito.verify(bulkProcessImageQCRepository)
            .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(Constant.STORE_ID,
                new BulkProcess().getBulkProcessCode());
    }

    @Test
    public void fetchBulkProcessImageQCByBulkProcessCodeAndParentProductTest() {
        bulkProcessImageQCServiceBean.fetchBulkProcessImageQCByBulkProcessCodeAndParentProduct(
            Constant.STORE_ID, BULK_PROCESS_CODE, PARENT_PRODUCT);
        Mockito.verify(bulkProcessImageQCRepository)
            .findByStoreIdAndBulkProcessCodeAndParentProductAndMarkForDeleteFalse(Constant.STORE_ID,
                BULK_PROCESS_CODE, PARENT_PRODUCT);
    }
    @Test
    public void saveBulkProcessImageQcTest() {
        bulkProcessImageQCServiceBean.saveBulkProcessImageQc(List.of(bulkProcessImageQC));
        Mockito.verify(bulkProcessImageQCRepository).saveAll(List.of(bulkProcessImageQC));
    }
}
