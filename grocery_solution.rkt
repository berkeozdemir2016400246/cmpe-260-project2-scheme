#lang scheme
;2016400246

(define recurse_list(lambda(name list) (if(memq name (car list))
                                    (car list)
                                    (if(eqv? (length list) 1)
                                     '(- 0 ())
                                     (recurse_list name (cdr list))
                                     ))
                      ))

(define adder(lambda (crops)(+ (caddr crops)(TRANSPORTATION-COST (cadr crops)))))
(define (in_farm_list farm_list)(lambda (crop) (memq (cadr crop) farm_list)))
(define buyable(lambda (customer crop)(filter (in_farm_list (CONTRACT-FARMS customer)) (filter (is_crop crop) CROPS))))
(define (farm_has_crop crop)(lambda(farm) (memq crop (AVAILABLE-CROPS farm))))
(define (crop_between high low)(lambda(crop_list)(let ([price (list-ref crop_list 2)])(and (<= price high) (>= price low))))) 
(define (is_crop crop)(lambda(crop_list)(memq crop crop_list)))
(define (int_crop crop)(lambda(customer)(memq crop (caddr customer))))
(define (has_farm farm)(lambda(customer)(memq farm (cadr customer))))
(define (map_nth list n)(map(lambda(items)(list-ref items n)) list))
(define (BUY-PRICE2 customer)(lambda(crop)(BUY-PRICE customer crop)))


(define TRANSPORTATION-COST(lambda(farm) (cadr(recurse_list farm FARMS))))

(define AVAILABLE-CROPS(lambda(farm) (caddr(recurse_list farm FARMS))))

(define INTERESTED-CROPS(lambda(customer) (caddr(recurse_list customer CUSTOMERS))))

(define CONTRACT-FARMS(lambda(customer) (if(eq? (cadr(recurse_list customer CUSTOMERS)) 0)
                                              '()
                                             (cadr(recurse_list customer CUSTOMERS)) )))

(define CONTRACT-WITH-FARM(lambda(farm) (map_nth (filter (has_farm farm) CUSTOMERS) 0)))

(define INTERESTED-IN-CROP(lambda(crop) (map_nth (filter (int_crop crop) CUSTOMERS) 0)))

(define MIN-SALE-PRICE(lambda(crop) (let ([vals (sort (map_nth (filter (is_crop crop) CROPS) 2) <)])
                                      (if (eqv? (length vals) 0)
                                          0
                                          (car vals)))))

(define CROPS-BETWEEN(lambda(low high) (remove-duplicates (map_nth (filter (crop_between high low) CROPS) 0))))

(define BUY-PRICE(lambda(customer crop) (let ([vals (sort (map adder (buyable customer crop)) <)])
                                      (if (eqv? (length vals) 0)
                                          0
                                          (car vals)))))

(define TOTAL-PRICE(lambda(customer) (apply + (map (BUY-PRICE2 customer) (INTERESTED-CROPS customer)))))