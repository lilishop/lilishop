package cn.lili.common.utils;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.StrUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 分页工具
 *
 * @author Chopper
 * @version v4.0
 * @Description:
 * @since 2020/11/26 15:23
 */
public class PageUtil {


    /**
     * Mybatis-Plus分页封装
     *
     * @param page
     * @return
     */
    public static <T> Page<T> initPage(PageVO page) {

        Page<T> p;
        int pageNumber = page.getPageNumber();
        int pageSize = page.getPageSize();
        String sort = page.getSort();
        String order = page.getOrder();

        if (pageNumber < 1) {
            pageNumber = 1;
        }
        if (pageSize < 1) {
            pageSize = 10;
        }
        if (pageSize > 100) {
            pageSize = 100;
        }
        if (StrUtil.isNotBlank(sort)) {
            Boolean isAsc = false;
            if (StrUtil.isBlank(order)) {
                isAsc = false;
            } else {
                if ("desc".equals(order.toLowerCase())) {
                    isAsc = false;
                } else if ("asc".equals(order.toLowerCase())) {
                    isAsc = true;
                }
            }
            p = new Page<>(pageNumber, pageSize);
            if (isAsc) {
                p.addOrder(OrderItem.asc(sort));
            } else {
                p.addOrder(OrderItem.desc(sort));
            }

        } else {
            p = new Page<>(pageNumber, pageSize);
        }
        return p;
    }

    /**
     * 生成条件搜索 全对象对比 equals
     * 如果需要like 需要另行处理
     *
     * @param object
     * @return
     */
    public static <T> QueryWrapper<T> initWrapper(Object object) {
        return initWrapper(object, null);
    }

    /**
     * 生成条件搜索 全对象对比
     *
     * @param object
     * @param searchVo
     * @return
     */
    public static <T> QueryWrapper<T> initWrapper(Object object, SearchVO searchVo) {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        //创建时间区间判定
        if (searchVo != null && StrUtil.isNotBlank(searchVo.getStartDate()) && StrUtil.isNotBlank(searchVo.getEndDate())) {
            Date start = cn.hutool.core.date.DateUtil.parse(searchVo.getStartDate());
            Date end = cn.hutool.core.date.DateUtil.parse(searchVo.getEndDate());
            queryWrapper.between("create_time", start, DateUtil.endOfDay(end));
        }
        if (object != null) {
            String[] fieldNames = BeanUtil.getFiledName(object);
            //遍历所有属性
            for (int j = 0; j < fieldNames.length; j++) {
                //获取属性的名字
                String key = fieldNames[j];
                //获取值
                Object value = BeanUtil.getFieldValueByName(key, object);
                //如果值不为空才做查询处理
                if (value != null && !"".equals(value)) {
                    //字段数据库中，驼峰转下划线
                    queryWrapper.eq(StringUtils.camel2Underline(key), value);
                }
            }
        }
        return queryWrapper;
    }


    /**
     * List 手动分页
     *
     * @param page
     * @param list
     * @return
     */
    public static <T> List<T> listToPage(PageVO page, List<T> list) {

        int pageNumber = page.getPageNumber() - 1;
        int pageSize = page.getPageSize();

        if (pageNumber < 0) {
            pageNumber = 0;
        }
        if (pageSize < 1) {
            pageSize = 10;
        }
        if (pageSize > 100) {
            pageSize = 100;
        }

        int fromIndex = pageNumber * pageSize;
        int toIndex = pageNumber * pageSize + pageSize;

        if (fromIndex > list.size()) {
            return new ArrayList<>();
        } else if (toIndex >= list.size()) {
            return list.subList(fromIndex, list.size());
        } else {
            return list.subList(fromIndex, toIndex);
        }
    }
}
