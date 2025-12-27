package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import static org.mockito.ArgumentMatchers.any;
import com.gdn.mta.bulk.entity.KafkaEventLog;
import com.gdn.mta.bulk.repository.KafkaEventLogRepository;

public class KafkaEventLogServiceBeanTest {

  private List<KafkaEventLog> kafkaEventLogList;

  @InjectMocks
  private KafkaEventLogServiceBean kafkaEventLogServiceBean;

  @Mock
  private KafkaEventLogRepository kafkaEventLogRepository;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    kafkaEventLogServiceBean = new KafkaEventLogServiceBean();
    kafkaEventLogServiceBean.setKafkaEventLogRepository(kafkaEventLogRepository);

    KafkaEventLog kafkaEventLog = new KafkaEventLog();
    kafkaEventLog.setTopicName("com.gdn.x.productcategorybase.category.publish");
    kafkaEventLog.setEventMessage("category change event");
    kafkaEventLogList = new ArrayList<>();
    kafkaEventLogList.add(kafkaEventLog);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(kafkaEventLogRepository);
  }

  @Test
  public void getKafkaEventLogsTest() {
    Mockito.when(this.kafkaEventLogRepository.findAll()).thenReturn(kafkaEventLogList);
    this.kafkaEventLogServiceBean.getKafkaEventLogs();
    Mockito.verify(this.kafkaEventLogRepository).findAll();
  }

  @Test
  public void getKafkaEventLogsNullValueTest() {
    Mockito.when(this.kafkaEventLogRepository.findAll()).thenReturn(null);
    this.kafkaEventLogServiceBean.getKafkaEventLogs();
    Mockito.verify(this.kafkaEventLogRepository).findAll();
  }

  @Test
  public void deleteKafkaEventLogsMarkForDeleteTest() {
    Mockito.doNothing().when(this.kafkaEventLogRepository).delete(any(KafkaEventLog.class));
    this.kafkaEventLogServiceBean.deleteKafkaEventLogsMarkForDelete(kafkaEventLogList);
    Mockito.verify(this.kafkaEventLogRepository).delete(any(KafkaEventLog.class));
  }

  @Test
  public void deleteKafkaEventLogsMarkForDeleteNullTest() {
    Mockito.doNothing().when(this.kafkaEventLogRepository).delete(any(KafkaEventLog.class));
    this.kafkaEventLogServiceBean.deleteKafkaEventLogsMarkForDelete(null);
  }
}
