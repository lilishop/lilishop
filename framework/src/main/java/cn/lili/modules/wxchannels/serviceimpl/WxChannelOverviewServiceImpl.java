package cn.lili.modules.wxchannels.serviceimpl;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.wxchannels.entity.dos.WxChannelOrder;
import cn.lili.modules.wxchannels.entity.dos.WxChannelRefund;
import cn.lili.modules.wxchannels.entity.vo.WxChannelOverviewDailyVO;
import cn.lili.modules.wxchannels.entity.vo.WxChannelOverviewSummaryVO;
import cn.lili.modules.wxchannels.service.WxChannelOverviewService;
import cn.lili.modules.wxchannels.service.WxChannelOrderService;
import cn.lili.modules.wxchannels.service.WxChannelRefundService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.net.URLEncoder;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class WxChannelOverviewServiceImpl implements WxChannelOverviewService {

    @Autowired
    private WxChannelOrderService orderService;
    @Autowired
    private WxChannelRefundService refundService;

    @Override
    public WxChannelOverviewSummaryVO summary(Long startTime, Long endTime) {
        DateRange range = rangeOrDefault(startTime, endTime);
        List<WxChannelOrder> orders = orderService.list(rangeWrapper(range));
        List<WxChannelRefund> refunds = refundService.list(rangeRefundWrapper(range));
        WxChannelOverviewSummaryVO vo = new WxChannelOverviewSummaryVO();
        vo.setTotalSales(sum(orders, null));
        vo.setLiveSales(sum(orders, "LIVE"));
        vo.setWindowSales(sum(orders, "WINDOW"));
        vo.setTotalRefund(sumRefund(refunds, null));
        vo.setLiveRefund(sumRefund(refunds, "LIVE"));
        vo.setWindowRefund(sumRefund(refunds, "WINDOW"));
        return vo;
    }

    @Override
    public List<WxChannelOverviewDailyVO> daily(Long startTime, Long endTime) {
        DateRange range = rangeOrDefault(startTime, endTime);
        List<WxChannelOrder> orders = orderService.list(rangeWrapper(range));
        List<WxChannelRefund> refunds = refundService.list(rangeRefundWrapper(range));
        Map<String, List<WxChannelOrder>> orderGroup = orders.stream().collect(Collectors.groupingBy(this::dayKey));
        Map<String, List<WxChannelRefund>> refundGroup = refunds.stream().collect(Collectors.groupingBy(this::dayKeyRefund));
        List<String> days = enumerateDays(range.start, range.end);
        List<WxChannelOverviewDailyVO> list = new ArrayList<>();
        for (String d : days) {
            WxChannelOverviewDailyVO vo = new WxChannelOverviewDailyVO();
            vo.setDate(d);
            vo.setTotalSales(sum(orderGroup.get(d), null));
            vo.setLiveSales(sum(orderGroup.get(d), "LIVE"));
            vo.setWindowSales(sum(orderGroup.get(d), "WINDOW"));
            vo.setTotalRefund(sumRefund(refundGroup.get(d), null));
            vo.setLiveRefund(sumRefund(refundGroup.get(d), "LIVE"));
            vo.setWindowRefund(sumRefund(refundGroup.get(d), "WINDOW"));
            list.add(vo);
        }
        list.sort(Comparator.comparing(WxChannelOverviewDailyVO::getDate).reversed());
        return list;
    }

    @Override
    public void export(HttpServletResponse response, Long startTime, Long endTime) {
        List<WxChannelOverviewDailyVO> list = daily(startTime, endTime);
        XSSFWorkbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet("视频号日报");
        Row header = sheet.createRow(0);
        String[] headers = {"日期", "视频号总销售额", "直播间销售额", "橱窗销售额", "视频号退货金额", "直播间退货金额", "橱窗退货金额"};
        for (int i = 0; i < headers.length; i++) {
            header.createCell(i).setCellValue(headers[i]);
        }
        for (int i = 0; i < list.size(); i++) {
            WxChannelOverviewDailyVO vo = list.get(i);
            Row row = sheet.createRow(i + 1);
            row.createCell(0).setCellValue(vo.getDate());
            row.createCell(1).setCellValue(opt(vo.getTotalSales()));
            row.createCell(2).setCellValue(opt(vo.getLiveSales()));
            row.createCell(3).setCellValue(opt(vo.getWindowSales()));
            row.createCell(4).setCellValue(opt(vo.getTotalRefund()));
            row.createCell(5).setCellValue(opt(vo.getLiveRefund()));
            row.createCell(6).setCellValue(opt(vo.getWindowRefund()));
        }
        try {
            String fileName = URLEncoder.encode("视频号报表", "UTF-8");
            response.setContentType("application/vnd.ms-excel;charset=UTF-8");
            response.setHeader("Content-Disposition", "attachment;filename=" + fileName + ".xlsx");
            ServletOutputStream out = response.getOutputStream();
            workbook.write(out);
        } catch (Exception e) {
        } finally {
            try {
                workbook.close();
            } catch (Exception e) {
            }
        }
    }

    private double opt(Double v) {
        return v == null ? 0d : v;
    }

    private LambdaQueryWrapper<WxChannelOrder> rangeWrapper(DateRange range) {
        LambdaQueryWrapper<WxChannelOrder> wrapper = new LambdaQueryWrapper<>();
        wrapper.between(WxChannelOrder::getCreateTime, range.start, range.end);
        wrapper.eq(WxChannelOrder::getDeleteFlag, false);
        return wrapper;
    }

    private LambdaQueryWrapper<WxChannelRefund> rangeRefundWrapper(DateRange range) {
        LambdaQueryWrapper<WxChannelRefund> wrapper = new LambdaQueryWrapper<>();
        wrapper.between(WxChannelRefund::getCreateTime, range.start, range.end);
        wrapper.eq(WxChannelRefund::getDeleteFlag, false);
        return wrapper;
    }

    private Double sum(List<WxChannelOrder> src, String scene) {
        if (src == null || src.isEmpty()) {
            return 0d;
        }
        return src.stream().filter(o -> CharSequenceUtil.isEmpty(scene) || CharSequenceUtil.equals(scene, o.getScene()))
                .map(WxChannelOrder::getAmount).filter(Objects::nonNull).mapToDouble(Double::doubleValue).sum();
    }

    private Double sumRefund(List<WxChannelRefund> src, String scene) {
        if (src == null || src.isEmpty()) {
            return 0d;
        }
        return src.stream().filter(o -> CharSequenceUtil.isEmpty(scene) || CharSequenceUtil.equals(scene, o.getScene()))
                .map(WxChannelRefund::getAmount).filter(Objects::nonNull).mapToDouble(Double::doubleValue).sum();
    }

    private String dayKey(WxChannelOrder o) {
        return DateUtil.formatDate(o.getCreateTime());
    }

    private String dayKeyRefund(WxChannelRefund o) {
        return DateUtil.formatDate(o.getCreateTime());
    }

    private List<String> enumerateDays(Date start, Date end) {
        LocalDate s = start.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
        LocalDate e = end.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
        List<String> res = new ArrayList<>();
        for (LocalDate d = s; !d.isAfter(e); d = d.plusDays(1)) {
            res.add(d.toString());
        }
        return res;
    }

    private DateRange rangeOrDefault(Long startTime, Long endTime) {
        Date end = endTime == null ? new Date() : new Date(endTime);
        Calendar cal = Calendar.getInstance();
        cal.setTime(end);
        cal.add(Calendar.DAY_OF_MONTH, -6);
        Date start = startTime == null ? Date.from(cal.toInstant()) : new Date(startTime);
        Calendar c = Calendar.getInstance();
        c.setTime(end);
        c.set(Calendar.HOUR_OF_DAY, 23);
        c.set(Calendar.MINUTE, 59);
        c.set(Calendar.SECOND, 59);
        c.set(Calendar.MILLISECOND, 999);
        end = c.getTime();
        Calendar s = Calendar.getInstance();
        s.setTime(start);
        s.set(Calendar.HOUR_OF_DAY, 0);
        s.set(Calendar.MINUTE, 0);
        s.set(Calendar.SECOND, 0);
        s.set(Calendar.MILLISECOND, 0);
        start = s.getTime();
        return new DateRange(start, end);
    }

    private static class DateRange {
        final Date start;
        final Date end;
        DateRange(Date s, Date e) { this.start = s; this.end = e; }
    }
}
