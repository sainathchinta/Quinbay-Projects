package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.TimeZone;

import org.joda.time.DateTime;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.FormulaUtilImpl;

public class FormulaUtilImplTest {

  private static final int DAY = 5;

  private static final int MONTH = 4;

  private static final int YEAR = 2011;

  private static final String CODE3 = "code3";

  private static final String DESCRIPTION = "description";

  private static final String CODE2 = "code2";

  private static final String CODE1 = "code1";

  private static final String STORE_ID = "storeId";

  private static final String BASE = "base";

  @InjectMocks
  public FormulaUtilImpl formulaUtilImpl;

  @Mock
  private SystemParameterService systemParameterService;

  @Test
  public void appendWithSerialTest() {
    String result = this.formulaUtilImpl.appendWithSerial(FormulaUtilImplTest.BASE, 5, 5);
    assertEquals(result, "base-00005");
  }

  @Test
  public void getConcurrentSizeBiggerThanSetSizeTest() {
    HashSet<String> codes = new HashSet<String>();
    codes.add(FormulaUtilImplTest.CODE1);
    codes.add(FormulaUtilImplTest.CODE2);
    when(
        this.systemParameterService.findValueByStoreIdAndVariable(FormulaUtilImplTest.STORE_ID,
            SystemParameterNames.MASTER_DATA_CONCURRENT_SIZE)).thenReturn(
        new SystemParameter(FormulaUtilImplTest.STORE_ID, FormulaUtilImplTest.CODE2, "3",
            FormulaUtilImplTest.DESCRIPTION));
    int result = this.formulaUtilImpl.getConcurrentSize(FormulaUtilImplTest.STORE_ID, codes);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(FormulaUtilImplTest.STORE_ID,
        SystemParameterNames.MASTER_DATA_CONCURRENT_SIZE);
    assertEquals(result, 2);
  }

  @Test
  public void getConcurrentSizeExceptionTest() {
    HashSet<String> codes = new HashSet<String>();
    codes.add(FormulaUtilImplTest.CODE1);
    codes.add(FormulaUtilImplTest.CODE2);
    codes.add(FormulaUtilImplTest.CODE3);
    when(
        this.systemParameterService.findValueByStoreIdAndVariable(FormulaUtilImplTest.STORE_ID,
            SystemParameterNames.MASTER_DATA_CONCURRENT_SIZE)).thenThrow(
        new ApplicationRuntimeException());
    int result = this.formulaUtilImpl.getConcurrentSize(FormulaUtilImplTest.STORE_ID, codes);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(FormulaUtilImplTest.STORE_ID,
        SystemParameterNames.MASTER_DATA_CONCURRENT_SIZE);
    assertEquals(result, 1);
  }

  @Test
  public void getConcurrentSizeLowerThanSetSizeTest() {
    HashSet<String> codes = new HashSet<String>();
    codes.add(FormulaUtilImplTest.CODE1);
    codes.add(FormulaUtilImplTest.CODE2);
    codes.add(FormulaUtilImplTest.CODE3);
    when(
        this.systemParameterService.findValueByStoreIdAndVariable(FormulaUtilImplTest.STORE_ID,
            SystemParameterNames.MASTER_DATA_CONCURRENT_SIZE)).thenReturn(
        new SystemParameter(FormulaUtilImplTest.STORE_ID, FormulaUtilImplTest.CODE2, "2",
            FormulaUtilImplTest.DESCRIPTION));
    int result = this.formulaUtilImpl.getConcurrentSize(FormulaUtilImplTest.STORE_ID, codes);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(FormulaUtilImplTest.STORE_ID,
        SystemParameterNames.MASTER_DATA_CONCURRENT_SIZE);
    assertEquals(result, 2);
  }

  @Test
  public void getConcurrentSizeOneTest() {
    HashSet<String> codes = new HashSet<String>();
    codes.add(FormulaUtilImplTest.CODE1);
    int concurrentSize =
        this.formulaUtilImpl.getConcurrentSize(FormulaUtilImplTest.STORE_ID, codes);
    assertEquals(concurrentSize, 1);
  }

  @Test
  public void getEndDate() {
    DateTime endDate =
        this.formulaUtilImpl.getEndDate(FormulaUtilImplTest.MONTH, FormulaUtilImplTest.YEAR);
    Calendar cal =
        new GregorianCalendar(FormulaUtilImplTest.YEAR, FormulaUtilImplTest.MONTH, 31, 23, 59, 59);
    cal.set(Calendar.MILLISECOND, 999);
    cal.setTimeZone(TimeZone.getTimeZone("UTC"));
    assertEquals(endDate.toDate(), cal.getTime());
  }

  @Test
  public void getEndOfDay() {
    DateTime endOfDay =
        this.formulaUtilImpl.getEndOfDay(FormulaUtilImplTest.DAY, FormulaUtilImplTest.MONTH,
            FormulaUtilImplTest.YEAR);
    Calendar cal =
        new GregorianCalendar(FormulaUtilImplTest.YEAR, FormulaUtilImplTest.MONTH,
            FormulaUtilImplTest.DAY, 23, 59, 59);
    cal.set(Calendar.MILLISECOND, 999);
    cal.setTimeZone(TimeZone.getTimeZone("UTC"));
    assertEquals(endOfDay.toDate(), cal.getTime());
  }

  @Test
  public void getStartDate() {
    DateTime startDate =
        this.formulaUtilImpl.getStartDate(FormulaUtilImplTest.MONTH, FormulaUtilImplTest.YEAR);
    Calendar cal = new GregorianCalendar(FormulaUtilImplTest.YEAR, FormulaUtilImplTest.MONTH, 1);
    cal.setTimeZone(TimeZone.getTimeZone("UTC"));
    assertEquals(startDate.toDate(), cal.getTime());
  }

  @Test
  public void getStartOfDay() {
    DateTime startOfDay =
        this.formulaUtilImpl.getStartOfDay(FormulaUtilImplTest.DAY, FormulaUtilImplTest.MONTH,
            FormulaUtilImplTest.YEAR);
    Calendar cal =
        new GregorianCalendar(FormulaUtilImplTest.YEAR, FormulaUtilImplTest.MONTH,
            FormulaUtilImplTest.DAY);
    cal.setTimeZone(TimeZone.getTimeZone("UTC"));
    assertEquals(startOfDay.toDate(), cal.getTime());
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.systemParameterService);
  }

}
