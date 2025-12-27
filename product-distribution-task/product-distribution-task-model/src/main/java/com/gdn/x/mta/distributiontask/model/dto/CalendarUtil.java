package com.gdn.x.mta.distributiontask.model.dto;

/**
 * Created by virajjasani on 24/10/16.
 */
public class CalendarUtil {

  private int startDayOfMonth;
  private int startHourOfDay;
  private int startMinute;
  private int startSecond;
  private int endDayOfMonth;
  private int endHourOfDay;
  private int endMinute;
  private int endSecond;

  public CalendarUtil() {
    // no implementation
  }

  public CalendarUtil(Builder builder) {
    this.startDayOfMonth = builder.startDayOfMonth;
    this.startHourOfDay = builder.startHourOfDay;
    this.startMinute = builder.startMinute;
    this.startSecond = builder.startSecond;
    this.endDayOfMonth = builder.endDayOfMonth;
    this.endHourOfDay = builder.endHourOfDay;
    this.endMinute = builder.endMinute;
    this.endSecond = builder.endSecond;
  }

  public int getStartDayOfMonth() {
    return startDayOfMonth;
  }

  public void setStartDayOfMonth(int startDayOfMonth) {
    this.startDayOfMonth = startDayOfMonth;
  }

  public int getStartHourOfDay() {
    return startHourOfDay;
  }

  public void setStartHourOfDay(int startHourOfDay) {
    this.startHourOfDay = startHourOfDay;
  }

  public int getStartMinute() {
    return startMinute;
  }

  public void setStartMinute(int startMinute) {
    this.startMinute = startMinute;
  }

  public int getStartSecond() {
    return startSecond;
  }

  public void setStartSecond(int startSecond) {
    this.startSecond = startSecond;
  }

  public int getEndDayOfMonth() {
    return endDayOfMonth;
  }

  public void setEndDayOfMonth(int endDayOfMonth) {
    this.endDayOfMonth = endDayOfMonth;
  }

  public int getEndHourOfDay() {
    return endHourOfDay;
  }

  public void setEndHourOfDay(int endHourOfDay) {
    this.endHourOfDay = endHourOfDay;
  }

  public int getEndMinute() {
    return endMinute;
  }

  public void setEndMinute(int endMinute) {
    this.endMinute = endMinute;
  }

  public int getEndSecond() {
    return endSecond;
  }

  public void setEndSecond(int endSecond) {
    this.endSecond = endSecond;
  }

  public static class Builder {
    private int startDayOfMonth;
    private int startHourOfDay;
    private int startMinute;
    private int startSecond;
    private int endDayOfMonth;
    private int endHourOfDay;
    private int endMinute;
    private int endSecond;

    public Builder setStartDayOfMonth(int startDayOfMonth) {
      this.startDayOfMonth = startDayOfMonth;
      return this;
    }

    public Builder setStartHourOfDay(int startHourOfDay) {
      this.startHourOfDay = startHourOfDay;
      return this;
    }

    public Builder setStartMinute(int startMinute) {
      this.startMinute = startMinute;
      return this;
    }

    public Builder setStartSecond(int startSecond) {
      this.startSecond = startSecond;
      return this;
    }

    public Builder setEndDayOfMonth(int endDayOfMonth) {
      this.endDayOfMonth = endDayOfMonth;
      return this;
    }

    public Builder setEndHourOfDay(int endHourOfDay) {
      this.endHourOfDay = endHourOfDay;
      return this;
    }

    public Builder setEndMinute(int endMinute) {
      this.endMinute = endMinute;
      return this;
    }

    public Builder setEndSecond(int endSecond) {
      this.endSecond = endSecond;
      return this;
    }

    public CalendarUtil build() {
      return new CalendarUtil(this);
    }
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("CalendarUtil{");
    sb.append("startDayOfMonth=").append(startDayOfMonth);
    sb.append(", startHourOfDay=").append(startHourOfDay);
    sb.append(", startMinute=").append(startMinute);
    sb.append(", startSecond=").append(startSecond);
    sb.append(", endDayOfMonth=").append(endDayOfMonth);
    sb.append(", endHourOfDay=").append(endHourOfDay);
    sb.append(", endMinute=").append(endMinute);
    sb.append(", endSecond=").append(endSecond);
    sb.append('}');
    return sb.toString();
  }
}
